package fr.acinq.bitcoin

import java.io.{ByteArrayInputStream, InputStream, OutputStream}
import java.nio.ByteOrder


import Protocol._
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.Crypto._
import fr.acinq.bitcoin.DeterministicWallet.KeyPath
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Script.Runner

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * see https://github.com/achow101/bips/blob/bip174-rev/bip-0174.mediawiki
  */
object PSBT {

  type Script = List[ScriptElt]

  case class KeyPathWithFingerprint(fingerprint: Long, hdKeyPath: KeyPath){
    override def toString: String = s"Fingerprint: ${BinaryData(writeUInt32(fingerprint))} KeyPath: $hdKeyPath"
  }

  case class MapEntry(key: BinaryData, value: BinaryData)

  object MapEntry {
    def apply(key: Int, value: BinaryData): MapEntry = new MapEntry(Seq(key.byteValue), value)
  }

  case class PartiallySignedInput(
    nonWitnessOutput: Option[Transaction] = None,
    witnessOutput:Option[TxOut] = None,
    redeemScript: Option[Script] = None,
    witnessScript: Option[Script] = None,
    finalScriptSig: Option[Script] = None,
    finalScriptWitness: Option[ScriptWitness] = None,
    bip32Data: Map[PublicKey, KeyPathWithFingerprint] = Map.empty,
    partialSigs: Map[PublicKey, BinaryData] = Map.empty,
    sighashType: Option[Int] = None,
    unknowns: Seq[MapEntry] = Seq.empty
  ) {

    require( !(witnessOutput.isDefined && nonWitnessOutput.isDefined), "PSBT Input can't have both witness and non withness UTXO")
    require( !(witnessScript.isDefined && witnessOutput.isEmpty), "PSBT Input with witness script must have witness output")
    require( !(finalScriptWitness.isDefined && witnessOutput.isEmpty), "PSBT Input with final script witness must have witness output")

    /**
      * Merges together 2 PSBT inputs, if a witness output is found the non witness is cleared.
      *
      * @param psbtIn
      * @return
      */
    def merge(psbtIn: PartiallySignedInput): PartiallySignedInput = {

      //see logic in sign.cpp#PSBTInput::Merge
      val (nonWitnessUtxo, witnessUtxo) = (nonWitnessOutput, witnessOutput) match {
        case (_, Some(_))       => (None, witnessOutput)
        case (Some(_), None)    => if(psbtIn.witnessOutput.isDefined) (None, psbtIn.witnessOutput) else (nonWitnessOutput, None)
        case (None, None)       => if(psbtIn.witnessOutput.isDefined) (None, psbtIn.witnessOutput) else (None, None)
      }

      PartiallySignedInput(
        nonWitnessOutput = nonWitnessUtxo,
        witnessOutput = witnessUtxo,
        redeemScript = if(redeemScript.isEmpty) psbtIn.redeemScript else redeemScript,
        witnessScript = if(witnessScript.isEmpty) psbtIn.witnessScript else witnessScript,
        finalScriptSig = if(finalScriptSig.isEmpty) psbtIn.finalScriptSig else finalScriptSig,
        finalScriptWitness = if(finalScriptWitness.isEmpty) psbtIn.finalScriptWitness else finalScriptWitness,
        partialSigs = partialSigs ++ psbtIn.partialSigs,
        sighashType = sighashType.orElse(psbtIn.sighashType),
        bip32Data = bip32Data ++ psbtIn.bip32Data,
        unknowns = removeDuplicatesFromPsbtMap(unknowns ++ psbtIn.unknowns)
      )
    }

    def hasFinalSigs: Boolean = finalScriptSig.isDefined || finalScriptWitness.isDefined

    def finalizeIfComplete(tx: Transaction, index: Int): PartiallySignedInput = {
      //this input has already been signed and its outputs scripts are complete
      if(hasFinalSigs) {
        return this
      }

      //Try to create finalized script
      (redeemScript, witnessScript) match {
        case (None, None) => this
        case (Some(redeem), None) =>
          val prevTx = nonWitnessOutput.getOrElse(throw new RuntimeException("Non witness output not found"))
          val utxo   = prevTx.txOut(index)
          val scriptPubKey = Script.parse(utxo.publicKeyScript)
          val pubkeyHash = BinaryData(Script.publicKeyHash(scriptPubKey))
          Script.isPayToScript(scriptPubKey) match {
            //P2SH
            case true => {
              //first step check the redeemScript hash
              assert(Crypto.hash160(Script.write(redeemScript.get)) == pubkeyHash, "P2SH redeem script does not match expected hash")
              val multiSigPubKeys = Script.publicKeysFromRedeemScript(redeem)
              val sigs = multiSigPubKeys.map(pubKeyData => partialSigs.get(PublicKey(pubKeyData))).filter(_.isDefined).flatten

              //The PSBT does not contain all the signatures necessary to redeem the script
              if (sigs.size < multiSigPubKeys.size) {
                return this
              }

              val scriptSig = (OP_0 +: sigs.map(OP_PUSHDATA(_))) ++ (OP_PUSHDATA(Script.write(redeem)) :: Nil)

              //Execute the unlocking script
              val runner = mkScriptRunner(tx, index)
              runner.verifyScripts(Script.write(scriptSig), Script.write(scriptPubKey)) match {
                case true => this.copy(
                  finalScriptSig = Some(scriptSig),
                  redeemScript = None,
                  witnessScript = None,
                  partialSigs = Map.empty,
                  bip32Data = Map.empty,
                  sighashType = None)
                case false => this
              }
            }
            //P2PKH
            case false => {
              partialSigs.find(el => el._1.hash160 == pubkeyHash) match {
                case None             =>  this //signatures were not found in the PSBT, unable to finalize this input
                case Some((pub, sig)) =>

                  val runner = mkScriptRunner(tx, index)
                  val scriptSig = OP_PUSHDATA(sig) :: OP_PUSHDATA(pub) :: Nil
                  Try(runner.verifyScripts(Script.write(scriptSig), Script.write(scriptPubKey))) match {
                    case Failure(thr)     =>
                      println(thr)
                      this
                    case Success(false)   => this
                    case Success(true)    => this.copy(
                      finalScriptSig = Some(scriptSig),
                      redeemScript = None,
                      partialSigs = Map.empty
                    )
                  }
              }
            }
          }
        // P2WPKH
        case (None, Some(witnessScriptCode)) =>
          throw new NotImplementedError("Pay-2-[Witness]-Public-Key-Hash not yet implemented")
        // P2WSH
        case (Some(redeem), Some(witness)) =>

          val utxo = witnessOutput.getOrElse(throw new IllegalArgumentException("Script pubkey not found"))
          val scriptPubKey = utxo.publicKeyScript

          val expectedHash = BinaryData(Script.publicKeyHash(scriptPubKey))
          val serializedRedeem = Script.write(redeem)
          assert(Crypto.hash160(serializedRedeem) == expectedHash, "P2SH redeem script does not match expected hash")

          val OP_PUSHDATA(data, dataLength) = redeem.last
          assert(dataLength == 32) //P2WSH
          assert(data == Crypto.sha256(Script.write(witness)), "SHA of the witness script must match the witness program")

          val pubKeys = Script.publicKeysFromRedeemScript(witness)
          val sigs = pubKeys.map(pubKeyData => partialSigs.get(PublicKey(pubKeyData))).filter(_.isDefined).flatten

          if(sigs.size < pubKeys.size){
            return this
          }

          val finalScriptSignature = OP_PUSHDATA(serializedRedeem) :: Nil
          val finalScriptWit = ScriptWitness(BinaryData("") +: sigs :+ Script.write(witness) )

          val runner = mkScriptRunner()
          runner.verifyScripts(Script.write(finalScriptSignature), scriptPubKey, finalScriptWit) match {
            case false  => this
            case true   => this.copy(
              finalScriptSig = Some(finalScriptSignature),
              finalScriptWitness = Some(finalScriptWit),
              redeemScript = None,
              witnessScript = None,
              sighashType = None,
              partialSigs = Map.empty,
              bip32Data = Map.empty
            )
          }

      }


    }


  }

  private lazy val dummyTx = Transaction(
    version = 1,
    TxIn(
      outPoint =  new OutPoint(BinaryData("0000000000000000000000000000000000000000000000000000000000000000"), 0),
      signatureScript = BinaryData("0000000000000000000000000000000000000000000000"),
      sequence = 0,
      witness = ScriptWitness.empty) +: Nil,
    TxOut(Satoshi(0), BinaryData("0000000000000000000000000000000000000000000000")) +: Nil,
    lockTime = 0
  )

  private def mkScriptRunner(
      tx: Transaction = dummyTx,
      inputIndex:Int = 0,
      amount: Satoshi = Satoshi(0),
      flags: Int = ScriptFlags.MANDATORY_SCRIPT_VERIFY_FLAGS): Script.Runner = {

    new Runner(Script.Context(tx, inputIndex, amount), flags)
  }

  case class PartiallySignedOutput(
    redeemScript: Option[Script] = None,
    witnessScript: Option[Script] = None,
    bip32Data: Map[PublicKey, KeyPathWithFingerprint] = Map.empty,
    unknowns: Seq[MapEntry] = Seq.empty
  ) {

    def merge(psbtOut: PartiallySignedOutput): PartiallySignedOutput = PartiallySignedOutput(
      redeemScript = if(redeemScript.isEmpty) psbtOut.redeemScript else redeemScript,
      witnessScript = if(witnessScript.isEmpty) psbtOut.witnessScript else witnessScript,
      bip32Data = bip32Data ++ psbtOut.bip32Data,
      unknowns = removeDuplicatesFromPsbtMap(unknowns ++ psbtOut.unknowns)
    )

  }

  case class PartiallySignedTransaction(
    tx: Transaction,
    inputs: Seq[PartiallySignedInput],
    outputs: Seq[PartiallySignedOutput],
    unknowns: Seq[MapEntry] = Seq.empty
  )

  final val PSBD_MAGIC = 0x70736274
  final val HEADER_SEPARATOR = 0xff
  final val SEPARATOR = 0x00

  object GlobalTypes extends Enumeration {
    val TransactionType = Value(0x00, "Transaction")
  }

  object InputTypes extends Enumeration {
    val NonWitnessUTXO = Value(0x00, "Non witness UTXO")
    val WitnessUTXO = Value(0x01, "Witness UTXO")
    val PartialSignature = Value(0x02, "Partial signature")
    val SighashType = Value(0x03, "Sighash")
    val RedeemScript = Value(0x04, "Redeem script")
    val WitnessScript = Value(0x05, "Witness script")
    val Bip32Data = Value(0x06, "Keypath for HD keys")
    val FinalScriptSig = Value(0x07, "Finalized scriptSig")
    val FinalScriptWitness = Value(0x08, "Finalized witness script")
  }

  object OutputTypes extends Enumeration {
    val RedeemScript = Value(0x00, "Redeem script")
    val WitnessScript = Value(0x01, "Witness script")
    val Bip32Data = Value(0x02, "Keypath for HD keys")
  }


  //Reads a list of key values, terminated by 0x00
  @tailrec
  private def readKeyValueMap(input: InputStream, acc: Seq[MapEntry] = Seq.empty): Seq[MapEntry] = {
    val keyLength = varint(input)
    if(keyLength == SEPARATOR) {
      return acc
    }

    val key = BinaryData(new Array[Byte](keyLength.toInt))
    input.read(key)

    val dataLength = varint(input)
    val data = BinaryData(new Array[Byte](dataLength.toInt))
    input.read(data)

    readKeyValueMap(input, acc :+ MapEntry(key, data))
  }

  //Reads a predetermined number of maps (as list of key/value records)
  @tailrec
  private def readMaps(counter: Int = 0, input: InputStream, acc: Seq[Seq[MapEntry]] = Seq.empty): Seq[Seq[MapEntry]] = {
    if(counter > 0) {
      readMaps(counter - 1, input, acc :+ readKeyValueMap(input))
    } else {
      acc
    }
  }

  private def removeDuplicatesFromPsbtMap(psbtMap: Seq[MapEntry]): Seq[MapEntry] = {
    //this converts the list of entries to a scala map and back to list of entries, scala maps enforce uniqueness on the key
    psbtMap.map( el => el.key -> el.value ).toMap.map{ case (k,v) => MapEntry(k, v) }.toSeq
  }

  private def assertNoDuplicates(psbtMap: Seq[MapEntry]) = {
    val setSmallerThanList = psbtMap.map(_.key).distinct.size < psbtMap.size
    assert(psbtMap.size < 2 || !setSmallerThanList, "Duplicate keys not allowed") //TODO add the key
  }

  private def isKeyUnknown[T <: Enumeration](key: BinaryData, enumType: T): Boolean = {
    !enumType.values.map(_.id).contains(key.head) // { 0x00, 0x01, 0x02, 0x03, 0x04 }
  }

  private def mapEntryToKeyPaths(entry: MapEntry):(PublicKey, KeyPathWithFingerprint) = {
    val pubKey = PublicKey(entry.key.drop(1))
    assert(isPubKeyValid(pubKey.data), "Invalid pubKey parsed")

    val derivationPaths = entry
      .value
      .grouped(4) //groups of 4 bytes (32 bit uint)
      .map(uint32(_, ByteOrder.LITTLE_ENDIAN))
      .toSeq

    //Store the first integer separately as fingerprint
    pubKey -> KeyPathWithFingerprint(derivationPaths.head, KeyPath(derivationPaths.tail))
  }

  private def mapEntryToScript(entry: MapEntry): Script = Script.parse(entry.value)

  def read64(input: String): PartiallySignedTransaction = {
    read(new ByteArrayInputStream(fromBase64String(input)))
  }

  def read(input: InputStream): PartiallySignedTransaction = {
    import GlobalTypes._
    import InputTypes._

    val psbtMagic = uint32(input, ByteOrder.BIG_ENDIAN)
    val separator = uint8(input)
    assert(psbtMagic == PSBD_MAGIC && separator == HEADER_SEPARATOR, s"PSBT header not valid '$psbtMagic|$separator'")

    //Read exactly one map for globals
    val globalMap = readKeyValueMap(input)

    val tx = globalMap.find(_.key.head == TransactionType.id) match {
      case Some(entry) => Transaction.read(entry.value)
      case None        => throw new IllegalArgumentException("PSBT requires one key-value entry for type Transaction")
    }

    tx.txIn.foreach { in =>
      assert(!in.hasSigScript && !in.hasWitness, s"Non empty input(${TxIn.write(in).toString}) found in the transaction")
    }

    val globalUnknowns = globalMap.filter(el => isKeyUnknown(el.key, GlobalTypes))

    //Read as many maps as the inputs/outpus found on the unsigned transaction
    val inputMaps = readMaps(tx.txIn.size, input)
    val outputMaps = readMaps(tx.txOut.size, input)

    //Assert there are no repeated keys within each maps's scope
    assertNoDuplicates(globalMap)
    inputMaps.foreach(assertNoDuplicates)
    outputMaps.foreach(assertNoDuplicates)

    val psbis = inputMaps.map { inputMap =>

      val redeemOut = inputMap.find(_.key.head == NonWitnessUTXO.id).map { nonWitnessUtxoEntry =>
        Transaction.read(nonWitnessUtxoEntry.value)
      }

      val witOut =  inputMap.find(_.key.head == WitnessUTXO.id).map { witnessUtxoEntry =>
        TxOut.read(witnessUtxoEntry.value)
      }

      val redeemScript = inputMap.find(_.key.head == RedeemScript.id).map(mapEntryToScript)
      val witScript = inputMap.find(_.key.head == WitnessScript.id).map(mapEntryToScript)
      val finRedeemScript = inputMap.find(_.key.head == FinalScriptSig.id).map(mapEntryToScript)
      val finWitScript = inputMap.find(_.key.head == FinalScriptWitness.id).map { finScriptWitnessEntry =>
        ScriptWitness.read(finScriptWitnessEntry.value)
      }

      val hdKeyPath = inputMap.filter(_.key.head == Bip32Data.id).map(mapEntryToKeyPaths).toMap

      val sigHash = inputMap.find(_.key.head == SighashType.id).map { sigHashEntry =>
        uint32(sigHashEntry.value, ByteOrder.LITTLE_ENDIAN).toInt
      }

      val partialSig = inputMap.filter(_.key.head == PartialSignature.id).map { partSigEntry =>
        PublicKey(partSigEntry.key.drop(1)) -> partSigEntry.value
      }.toMap

      val unknowns = inputMap.filter(el => isKeyUnknown(el.key, InputTypes))

      PartiallySignedInput(redeemOut, witOut, redeemScript, witScript, finRedeemScript, finWitScript, hdKeyPath, partialSig, sigHash, unknowns)
    }

    val psbtOuts = outputMaps.map { outputMap =>

      val redeemScript = outputMap.find(_.key.head == OutputTypes.RedeemScript.id).map(mapEntryToScript)
      val witScript = outputMap.find(_.key.head == OutputTypes.WitnessScript.id).map(mapEntryToScript)
      val hdKeyPaths = outputMap.filter(_.key.head == OutputTypes.Bip32Data.id).map(mapEntryToKeyPaths).toMap
      val unknowns = outputMap.filter(el => isKeyUnknown(el.key, InputTypes))

      PartiallySignedOutput(redeemScript, witScript, hdKeyPaths, unknowns)
    }

    PartiallySignedTransaction(tx, psbis, psbtOuts, globalUnknowns)
  }

  private def writeKeyValue(entry: MapEntry, out: OutputStream): Unit = {
    val keyLength = entry.key.size
    writeVarint(keyLength, out)
    writeBytes(entry.key, out)

    val valueLength = entry.value.size
    writeVarint(valueLength, out)
    writeBytes(entry.value, out)
  }

  def write(psbt: PartiallySignedTransaction, out: OutputStream): Unit = {
    import GlobalTypes._
    import InputTypes._

    writeUInt32(PSBD_MAGIC, out, ByteOrder.BIG_ENDIAN)
    writeUInt8(HEADER_SEPARATOR, out)

    val txEntry = MapEntry(Seq(TransactionType.id.toByte), Transaction.write(psbt.tx))

    //write global map
    writeKeyValue(txEntry, out)
    psbt.unknowns.foreach(writeKeyValue(_, out))
    writeUInt8(SEPARATOR, out)

    psbt.inputs.foreach { input =>

      val redeemOut = input.nonWitnessOutput.map(tx => MapEntry(NonWitnessUTXO.id, Transaction.write(tx)))
      val witOut = input.witnessOutput.map(txOut => MapEntry(WitnessUTXO.id, TxOut.write(txOut)))
      val redeemScript = input.redeemScript.map(script => MapEntry(RedeemScript.id, Script.write(script)))
      val witscript = input.witnessScript.map(wscript => MapEntry(WitnessScript.id, Script.write(wscript)))
      val finScriptSig = input.finalScriptSig.map(script => MapEntry(FinalScriptSig.id, Script.write(script)))
      val finScriptWit = input.finalScriptWitness.map{ wscript =>
        MapEntry(FinalScriptWitness.id, ScriptWitness.write(wscript))
      }
      val bip32Data = input.bip32Data.map { case (pubKey, keyPath) =>
        MapEntry(Bip32Data.id.byteValue +: pubKey.data, writeUInt32(keyPath.fingerprint) ++ keyPath.hdKeyPath.map(writeUInt32).flatten)
      }

      val partialSigs = input.partialSigs.map { case (pubKey, sig) =>
        MapEntry(PartialSignature.id.byteValue +: pubKey.data, sig)
      }

      val sigHash = input.sighashType.map { value =>
        MapEntry(SighashType.id, writeUInt32(value))
      }

      //Write to stream
      redeemOut.foreach(writeKeyValue(_, out))
      witOut.foreach(writeKeyValue(_, out))
      redeemScript.foreach(writeKeyValue(_, out))
      witscript.foreach(writeKeyValue(_, out))
      finScriptSig.foreach(writeKeyValue(_, out))
      finScriptWit.foreach(writeKeyValue(_, out))
      bip32Data.foreach(writeKeyValue(_, out))
      partialSigs.foreach(writeKeyValue(_, out))
      sigHash.foreach(writeKeyValue(_, out))
      input.unknowns.foreach(writeKeyValue(_, out))
      writeUInt8(SEPARATOR, out)

    }

    psbt.outputs.foreach { output =>

      val redeemScript = output.redeemScript.map(script => MapEntry(OutputTypes.RedeemScript.id, Script.write(script)))
      val witnessScript = output.witnessScript.map(wscript => MapEntry(OutputTypes.WitnessScript.id, Script.write(wscript)))
      val bip32Data = output.bip32Data.map { case (pubKey, keyPath) =>
        MapEntry(OutputTypes.Bip32Data.id.byteValue +: pubKey.data, writeUInt32(keyPath.fingerprint) ++ keyPath.hdKeyPath.map(writeUInt32).flatten)
      }

      redeemScript.foreach(writeKeyValue(_, out))
      witnessScript.foreach(writeKeyValue(_, out))
      bip32Data.foreach(writeKeyValue(_, out))
      output.unknowns.foreach(writeKeyValue(_, out))
      writeUInt8(SEPARATOR, out)

    }

  }

  def createPSBT(inputs: Seq[TxIn], outputs: Seq[TxOut], lockTime: Long = 0): PartiallySignedTransaction = {
    val tx = Transaction(version = 2, inputs, outputs, lockTime)

    PartiallySignedTransaction(
      tx = tx,
      inputs = tx.txIn.map(_ => PartiallySignedInput()),
      outputs = tx.txOut.map(_ => PartiallySignedOutput())
    )
  }

  def mergePSBT(firstPSBT: PartiallySignedTransaction, secondPSBT: PartiallySignedTransaction): PartiallySignedTransaction = {
    //check if they refer to the same transaction
    assert(firstPSBT.tx.hash == secondPSBT.tx.hash, "Unable to merge PSBTs, they don't refer to the same transction")

    //merged inputs
    val combinedInputs= firstPSBT.inputs.zipWithIndex.map{ case (in, idx) => in.merge(secondPSBT.inputs(idx)) }

    //merged outputs
    val combinedOutputs = firstPSBT.outputs.zipWithIndex.map{ case (out, idx) => out.merge(secondPSBT.outputs(idx)) }

    //merged unknowns
    val combinedUnknowns = removeDuplicatesFromPsbtMap(firstPSBT.unknowns ++ secondPSBT.unknowns)

    PartiallySignedTransaction(firstPSBT.tx, combinedInputs, combinedOutputs, combinedUnknowns)
  }

  def finalizePSBT(psbt: PartiallySignedTransaction): PartiallySignedTransaction = {

    //try to finalize the inputs if they've already been signed (partial sigs are exaustive)
    val updated = psbt.copy(inputs = psbt.inputs.zipWithIndex.map{ case (input, idx) => input.finalizeIfComplete(psbt.tx, idx) } )
    updated
//    var tx = updated.tx
//    updated.inputs.zipWithIndex.map { case (input, idx) =>
//      input match {
//        case in if !in.hasFinalSigs => in
//        case PartiallySignedInput(_,_,_,_,_,Some(scriptWitness),_,_,_,_) =>
//          tx = tx.updateWitness(idx, scriptWitness)
//        case PartiallySignedInput(_,_,_,_,Some(sigScript),None,_,_,_,_) =>
//          tx = tx.updateSigScript(idx, sigScript)
//        case PartiallySignedInput(_,_,_,_,Some(sigScript),Some(scriptWitness),_,_,_,_) =>
//          tx = tx.updateSigScript(idx, sigScript)
//          tx = tx.updateWitness(idx, scriptWitness)
//      }
//      ???
//    }
  }


}
