package fr.acinq.bitcoin.reference

import java.io.ByteArrayOutputStream
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin._
import fr.acinq.bitcoin.{PSBT, Transaction}
import fr.acinq.bitcoin.PSBT._
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization
import scala.io.Source
import scala.util.{Failure, Success, Try}

@RunWith(classOf[JUnitRunner])
class PSBTSpec extends FlatSpec {

  implicit val format = DefaultFormats

  case class PSBTTestData(
    invalid: Seq[String],
    valid: Seq[String],
    signer: Seq[SignerData],
    combiner: Seq[CombinerData],
    finalizer: Seq[FinalizerData],
    extractor: Seq[ExtractorData]
  )

  case class CombinerData(combine: Seq[String], result: String)
  case class FinalizerData(finaliza: String, result: String)
  case class ExtractorData(extract: String, result: String)
  case class SignerData(privkeys:Seq[String], psbt: String, result: String)


  //TODO check out commit 52d9adcdfbf545cb9a9facb482cb3673578abfa0
  "PSBT" should "pass the reference tests" in {
    val out = new ByteArrayOutputStream()

    val testData = Serialization.read[PSBTTestData](
      Source.fromFile("src/test/resources/data/psbt_test_data.json").bufferedReader
    )

    //test invalid
    testData.invalid.foreach { invalidPSBT=>
      assert(Try(PSBT.read64(invalidPSBT)).isFailure)
    }

    //test valid
    testData.valid.foreach { case validPSBT =>
      assert(Try(PSBT.read64(validPSBT)) match {
        case Success(_) => true
        //Try again forcing non segwit serialization format (#5 contains this transaction: 02000000000140420f000000000017a9146e91b72d5593e7d4391e2ff44e91e985c31641f08700000000)
        case Failure(_) => Try(PSBT.read64(validPSBT, 0x40000000L)).isSuccess
      })
    }

    //signer test
    testData.signer.foreach { signerTest =>

      val psbt = read64(signerTest.psbt)
      val keys = signerTest.privkeys.map(PrivateKey.fromBase58(_, Base58.Prefix.SecretKeyTestnet))

      val signed = PSBT.signPSBT(psbt, keys)

      out.reset()
      PSBT.write(signed, out)

      assert(toBase64String(out.toByteArray) == signerTest.result)

    }

    //combiner test
    testData.combiner.foreach { combinerTest =>
      val combined = combinerTest.combine.map(in => read64(in)).reduce(mergePSBT(_, _))

      out.reset()
      PSBT.write(combined, out)

      assert(toBase64String(out.toByteArray) == combinerTest.result)
    }

    //finalizer test
    testData.finalizer.foreach { finalizerTest =>

      val psbt = read64(finalizerTest.finaliza)
      val finalized = finalizePSBT(psbt)

      out.reset()
      PSBT.write(finalized, out)

      assert(toBase64String(out.toByteArray) == finalizerTest.result)
    }

    //extractor
    testData.extractor.foreach { extractorTest =>

      val extracted = extractPSBT(read64(extractorTest.extract))

      assert(Transaction.write(extracted).toString == extractorTest.result)
    }

  }

  ignore should "be fully compatible with the SIGNER reference implementation" in {
    val out = new ByteArrayOutputStream()

    val privKeys = Seq(
      "cT7J9YpCwY3AVRFSjN6ukeEeWY6mhpbJPxRaDaP5QTdygQRxP9Au",
      "cNBc3SWUip9PPm1GjRoLEJT6T41iNzCYtD7qro84FMnM5zEqeJsE"
    ).map(PrivateKey.fromBase58(_, Base58.Prefix.SecretKeyTestnet))

    val psbt = PSBT.read64("cHNidP8BAJoCAAAAAljoeiG1ba8MI76OcHBFbDNvfLqlyHV5JPVFiHuyq911AAAAAAD/////g40EJ9DsZQpoqka7CwmK6kQiwHGyyng1Kgd5WdB86h0BAAAAAP////8CcKrwCAAAAAAWABTYXCtx0AYLCcmIauuBXlCZHdoSTQDh9QUAAAAAFgAUAK6pouXw+HaliN9VRuh0LR2HAI8AAAAAAAEAuwIAAAABqtc5MQGL0l+ErkALaISL4J23BurCrBgpi6vucatlb4sAAAAASEcwRAIgWPb8fGoz4bMVSNSByCbAFb0wE1qtQs1neQ2rZtKtJDsCIEoc7SYExnNbY5PltBaR3XiwDwxZQvufdRhW+qk4FX26Af7///8CgPD6AgAAAAAXqRQPuUY0IWlrgsgzryQceMF9295JNIfQ8gonAQAAABepFCnKdPigj4GZlCgYXJe12FLkBj9hh2UAAAABBEdSIQKVg785rgpgl0etGZrd1jT6YQhVnWxc05tMIYPxq5bgfyEC2rYf9JoU22p9ArDNH7t4/EsYMStbTlTa5Nui+/71NtdSriIGApWDvzmuCmCXR60Zmt3WNPphCFWdbFzTm0whg/GrluB/ENkMak8AAACAAAAAgAAAAIAiBgLath/0mhTban0CsM0fu3j8SxgxK1tOVNrk26L7/vU21xDZDGpPAAAAgAAAAIABAACAAQMEAQAAAAABASAAwusLAAAAABepFLf1+vQOPUClpFmx2zU18rcvqSHohwEEIgAgjCNTFzdDtZXftKB7crqOQuN5fadOh/59nXSX47ICiQMBBUdSIQMIncEMesbbVPkTKa9hczPbOIzq0MIx9yM3nRuZAwsC3CECOt2QTz1tz1nduQaw3uI1Kbf/ue1Q5ehhUZJoYCIfDnNSriIGAjrdkE89bc9Z3bkGsN7iNSm3/7ntUOXoYVGSaGAiHw5zENkMak8AAACAAAAAgAMAAIAiBgMIncEMesbbVPkTKa9hczPbOIzq0MIx9yM3nRuZAwsC3BDZDGpPAAAAgAAAAIACAACAAQMEAQAAAAAiAgOppMN/WZbTqiXbrGtXCvBlA5RJKUJGCzVHU+2e7KWHcRDZDGpPAAAAgAAAAIAEAACAACICAn9jmXV9Lv9VoTatAsaEsYOLZVbl8bazQoKpS2tQBRCWENkMak8AAACAAAAAgAUAAIAA")
    val expectedRawPsbt = "cHNidP8BAJoCAAAAAljoeiG1ba8MI76OcHBFbDNvfLqlyHV5JPVFiHuyq911AAAAAAD/////g40EJ9DsZQpoqka7CwmK6kQiwHGyyng1Kgd5WdB86h0BAAAAAP////8CcKrwCAAAAAAWABTYXCtx0AYLCcmIauuBXlCZHdoSTQDh9QUAAAAAFgAUAK6pouXw+HaliN9VRuh0LR2HAI8AAAAAAAEAuwIAAAABqtc5MQGL0l+ErkALaISL4J23BurCrBgpi6vucatlb4sAAAAASEcwRAIgWPb8fGoz4bMVSNSByCbAFb0wE1qtQs1neQ2rZtKtJDsCIEoc7SYExnNbY5PltBaR3XiwDwxZQvufdRhW+qk4FX26Af7///8CgPD6AgAAAAAXqRQPuUY0IWlrgsgzryQceMF9295JNIfQ8gonAQAAABepFCnKdPigj4GZlCgYXJe12FLkBj9hh2UAAAAiAgLath/0mhTban0CsM0fu3j8SxgxK1tOVNrk26L7/vU210cwRAIgYxqYn+c4qSrQGYYCMxLBkhT+KAKznly8GsNniAbGksMCIDnbbDh70mdxbf2z1NjaULjoXSEzJrp8faqkwM5B65IjAQEDBAEAAAABBEdSIQKVg785rgpgl0etGZrd1jT6YQhVnWxc05tMIYPxq5bgfyEC2rYf9JoU22p9ArDNH7t4/EsYMStbTlTa5Nui+/71NtdSriIGApWDvzmuCmCXR60Zmt3WNPphCFWdbFzTm0whg/GrluB/ENkMak8AAACAAAAAgAAAAIAiBgLath/0mhTban0CsM0fu3j8SxgxK1tOVNrk26L7/vU21xDZDGpPAAAAgAAAAIABAACAAAEBIADC6wsAAAAAF6kUt/X69A49QKWkWbHbNTXyty+pIeiHIgICOt2QTz1tz1nduQaw3uI1Kbf/ue1Q5ehhUZJoYCIfDnNHMEQCIGX0W6WZi1mif/4ae+0BavHx+Q1Us6qPdFCqX1aiUQO9AiB/ckcDrR7blmgLKEtW1P/LiPf7dZ6rvgiqMPKbhROD0gEBAwQBAAAAAQQiACCMI1MXN0O1ld+0oHtyuo5C43l9p06H/n2ddJfjsgKJAwEFR1IhAwidwQx6xttU+RMpr2FzM9s4jOrQwjH3IzedG5kDCwLcIQI63ZBPPW3PWd25BrDe4jUpt/+57VDl6GFRkmhgIh8Oc1KuIgYCOt2QTz1tz1nduQaw3uI1Kbf/ue1Q5ehhUZJoYCIfDnMQ2QxqTwAAAIAAAACAAwAAgCIGAwidwQx6xttU+RMpr2FzM9s4jOrQwjH3IzedG5kDCwLcENkMak8AAACAAAAAgAIAAIAAIgIDqaTDf1mW06ol26xrVwrwZQOUSSlCRgs1R1Ptnuylh3EQ2QxqTwAAAIAAAACABAAAgAAiAgJ/Y5l1fS7/VaE2rQLGhLGDi2VW5fG2s0KCqUtrUAUQlhDZDGpPAAAAgAAAAIAFAACAAA=="

    val signed = PSBT.signPSBT(psbt, privKeys)

    PSBT.write(signed, out)

    assert(toBase64String(out.toByteArray) == expectedRawPsbt)

  }

  "Signatures" should "equal those produced by bitcoind" in {
    val bitcoindSigScript = BinaryData("0047304402201aaadac2eade5e3aa0d77ad788f7e98b868016dfc063f902bf8633fa4fa1696102205b419ff65d661b851ef406c89b7cdcd808617c3518f1ce01723c640d3a3418280147304402202f791e50d7f0f2ff991a1c5f669918e0c44b24d8904470d831e3cde28d6b1dc80220678392831b0cd670aa4834e2f0e96268caee33c06c8f283319073bb89b47b84901475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae")
    val redeemScript = BinaryData("5221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae")
    val amount = 10 btc
    val priv1 = PrivateKey.fromBase58("cP53pDbR5WtAD8dYAW9hhTjuvvTVaEiQBdrz9XPrgLBeRFiyCbQr", Base58.Prefix.SecretKeyTestnet)
    val priv2 = PrivateKey.fromBase58("cT7J9YpCwY3AVRFSjN6ukeEeWY6mhpbJPxRaDaP5QTdygQRxP9Au", Base58.Prefix.SecretKeyTestnet)

    val prevTx = Transaction.read("02000000019c2c4e7068de70dc0d9205b4828d0652541294e0b3d85fb3a9fe5f4c4e5873a60000000048473044022055456502ebfa9af1862c2328bd214f8c3fceca4a8d691f5f50136f8a14e1ecff02204f358bb078e8a202ebed91f8844e622ca21f86ceed276c7164f4c6e303d4db1401feffffff0200ca9a3b0000000017a9140fb9463421696b82c833af241c78c17ddbde493487e4a3e60e0000000017a9143d3c0f96f187c20cfb01f22c8e326dc612dc9718873a020000")

    val spendingTx = Transaction(
      version = 2,
      txIn = TxIn(OutPoint(prevTx, 0), sequence = 0xffffffffL, signatureScript = Nil) :: Nil,
      txOut = TxOut(amount, BinaryData("a9143454e2293ed9790e578ae1353ffe1ef1d4a6b34a87")) :: Nil,
      lockTime = 0
    )

    assert(spendingTx.txid.toString == "aa3c0b069e8a8571e2417cfe1140f2d8040f85d12b10ac01b4dcd29e6120bf11")

    val sig = Transaction.signInput(spendingTx, 0, redeemScript, SIGHASH_ALL, amount, SigVersion.SIGVERSION_BASE, priv1)
    val sig1 = Transaction.signInput(spendingTx, 0, redeemScript, SIGHASH_ALL, amount, SigVersion.SIGVERSION_BASE, priv2)

    println(s"SIG: $sig")
    println(s"[non-matching signature]SIG1: $sig1")

    val sigScript = OP_0 :: OP_PUSHDATA(sig) :: OP_PUSHDATA(sig1) :: OP_PUSHDATA(redeemScript) :: Nil

    assert(Script.write(sigScript) == bitcoindSigScript)

  }

}
