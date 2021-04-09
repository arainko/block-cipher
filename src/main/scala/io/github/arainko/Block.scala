import io.github.arainko.Cipher
import zio._

object Block extends App {

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (Cipher.ecb("files/plain.bmp") *> Cipher.cbc("files/plain.bmp")).exitCode

}
