package io.github.arainko

import io.github.arainko.model._
import scodec.bits.ByteVector
import zio._
import zio.blocking._
import zio.stream.ZStream

import java.io.File
import java.nio.file.Paths
import javax.imageio._

object Cipher {

  private val key = "qbcUR4OrBTPebHiSPRfv"
  private val iv  = List(119, 54, 80, 83, 49, 111, 48, 116, 65, 85, 51, 48, 90, 112, 69, 89, 113, 52, 116, 97)

  private def mutateImageWithKey(image: Image) =
    image.mutateBlocks(4, 5) { (blockIndex, argb) =>
      val keyChar = key(blockIndex)
      ByteVector
        .fromInt(argb)
        .digest("SHA-1")
        .xor(ByteVector.fromInt(keyChar.toInt))
        .toByteBuffer
        .getInt()
    }

  def ebc(path: String): ZIO[Blocking, Throwable, Boolean] =
    ZStream
      .fromFile(Paths.get(path))
      .toInputStream
      .map(Image.compose(ImageIO.read))
      .use { img =>
        ZIO.effect {
          mutateImageWithKey(img)
        } *> effectBlockingIO(ImageIO.write(img.image, "bmp", new File("files/ebc_crypto.bmp")))
      }

  def cbc(path: String): ZIO[Blocking, Throwable, Boolean] =
    ZStream
      .fromFile(Paths.get(path))
      .toInputStream
      .map(Image.compose(ImageIO.read))
      .use { img =>
        ZIO.effect {
          img.mutateBlocksWithLookback(4, 5, iv) { (prevValue, blockIndex, color) =>
            val argb    = prevValue ^ color
            val keyChar = key(blockIndex)
            ByteVector
              .fromInt(argb)
              .digest("SHA-1")
              .xor(ByteVector.fromInt(keyChar.toInt))
              .toByteBuffer
              .getInt()
          }
        } *> effectBlockingIO(ImageIO.write(img.image, "bmp", new File("files/cbc_crypto.bmp")))
      }

}
