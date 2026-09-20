package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

class GenerateKeyOptions(val modulusLength: Int) extends js.Object

class JwkExport(val format: String) extends js.Object

@js.native
trait JwkFields extends js.Object:
  val kty: js.UndefOr[String] = js.native
  val n: String               = js.native
  val e: String               = js.native
  val d: js.UndefOr[String]   = js.native
end JwkFields

class JwkKeyInput(val key: js.Dictionary[String], val format: String) extends js.Object

class SignKeyOptions(val key: KeyObject, var padding: js.UndefOr[Int] = js.undefined) extends js.Object

@js.native
trait Hash extends js.Object:
  def update(data: Uint8Array): Hash = js.native
  def digest(): Uint8Array           = js.native

@js.native
trait KeyObject extends js.Object:
  @js.annotation.JSName("export")
  def exportJwk(options: JwkExport): JwkFields = js.native

@js.native
trait KeyPair extends js.Object:
  val publicKey: KeyObject  = js.native
  val privateKey: KeyObject = js.native

@js.native
trait CryptoConstants extends js.Object:
  val RSA_PKCS1_PADDING: Int = js.native

@js.native
trait NodeBuf extends Uint8Array:
  def toString(encoding: String): String = js.native

@js.native
@JSImport("node:buffer", "Buffer")
private[heddle] object NodeBuffer extends js.Object:
  def from(str: String, encoding: String): NodeBuf = js.native
  def from(buf: Uint8Array): NodeBuf               = js.native

@js.native
@JSImport("node:crypto", JSImport.Namespace)
private[heddle] object Crypto extends js.Object:
  val constants: CryptoConstants = js.native

  def createHash(algorithm: String): Hash = js.native

  def generateKeyPairSync(typ: String, options: GenerateKeyOptions): KeyPair = js.native

  def createPrivateKey(opts: JwkKeyInput): KeyObject = js.native

  def createPublicKey(opts: JwkKeyInput): KeyObject = js.native

  def sign(algorithm: String, data: Uint8Array, key: SignKeyOptions): Uint8Array = js.native

  def verify(
      algorithm: String,
      data: Uint8Array,
      key: SignKeyOptions,
      signature: Uint8Array,
  ): Boolean = js.native
end Crypto
