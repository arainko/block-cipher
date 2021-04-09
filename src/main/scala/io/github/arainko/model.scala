package io.github.arainko

import io.github.arainko.syntax._

import java.awt.image.BufferedImage

object model {

  final case class Image(image: BufferedImage) extends AnyVal {
    def width: Int  = image.getWidth
    def height: Int = image.getHeight

    def mutateBlocks(stepWidth: Int, stepHeight: Int)(f: (Int, Int) => Int): Unit =
      for {
        startX  <- 0.until(width, stepWidth)
        startY  <- 0.until(height, stepHeight)
        offsetX <- startX.until(startX + stepWidth) if offsetX < width
        offsetY <- startY.until(startY + stepHeight) if offsetY < height
        blockIndex   = (offsetX - startX) + (offsetY - startY) * 3
        mutatedColor = f(blockIndex, image.getRGB(offsetX, offsetY))
      } image.setRGB(offsetX, offsetY, mutatedColor)

    def mutateBlocksWithLookback(stepWidth: Int, stepHeight: Int, initVector: Seq[Int])(
      f: (Int, Int, Int) => Int
    ): Unit =
      for {
        startX  <- 0.until(width, stepWidth)
        startY  <- 0.until(height, stepHeight)
        offsetX <- startX.until(startX + stepWidth) if offsetX < width
        offsetY <- startY.until(startY + stepHeight) if offsetY < height
        blockIndex     = (offsetX - startX) + (offsetY - startY) * 3
        (prevX, prevY) = calculatePreviousCords(offsetX, offsetY, stepWidth, stepHeight)
        prevBlockValue = if (prevX < 0) initVector(blockIndex) else image.getRGB(prevX, prevY)
        mutatedColor   = f(prevBlockValue, blockIndex, image.getRGB(offsetX, offsetY))
      } image.setRGB(offsetX, offsetY, mutatedColor)

    private def calculatePreviousCords(currX: Int, currY: Int, stepWidth: Int, stepHeight: Int) =
      (currY - stepHeight) match {
        case overflow if overflow < 0 => (currX - stepWidth, (currY - stepHeight) %% height)
        case _                        => (currX, currY - stepHeight)
      }
  }
}
