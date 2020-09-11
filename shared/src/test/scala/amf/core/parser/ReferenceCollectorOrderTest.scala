package amf.core.parser

import java.util.UUID

import org.scalatest.{FunSuite, Matchers}

class ReferenceCollectorOrderTest extends FunSuite with Matchers {

  test("Reference collector keeps reference order") {
    val testData  = randomUuids.zipWithIndex
    val collector = DefaultReferenceCollector[Integer]()
    testData.foreach(data => collector += (data._1, data._2))
    collector.references() shouldBe sorted
  }

  def randomUuids = Seq(
      "aed1db9f-9f5e-44e2-a181-1e8e42dadbc2",
      "e6904643-bfb9-4c15-b1b9-ce63fbfa1cf5",
      "816167f5-945b-496c-9ce1-9cb0dff6cc64",
      "eee9eddb-fc94-476e-aac3-5b075ba2c4ff",
      "46f497f8-1090-4090-af87-64f0684ea1f9",
      "57504629-4694-4986-87f0-a562ca58bca1",
      "525e7cc3-7eda-405b-8d72-f6f623f33f77",
      "a9eaf998-5bf5-4ed9-ad45-04a63fb7ac85",
      "62a2553e-8434-4237-a9ea-4fef6e44d8b7",
      "6373d855-85ed-437d-aead-296376458872",
      "37e3df3e-9416-417b-a0f1-f7bbcec29a43",
      "a4920432-2c93-49dd-a239-0493f1ca0469",
      "4a155f60-f579-4513-ba4a-95c1465dd605",
      "0deb2758-31fb-4c24-a909-110652c82711",
      "99481df4-31e4-4b1e-9667-756188806dc2",
      "efb6d7d1-f957-45de-8ad7-67097cec1d77",
      "a022242b-12c0-4469-a758-8616b0425172",
      "6205520d-6200-4ff1-9124-fac9f0e85d1d",
      "8de72975-cefd-469f-8b1a-8cbc8b42d94c"
  )
}
