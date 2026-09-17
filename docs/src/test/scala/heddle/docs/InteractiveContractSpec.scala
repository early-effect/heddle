package heddle.docs

import specular.*
import zio.test.*

/** JVM half of the mount contract: live illustration keys match the registry. */
object InteractiveContractSpec extends ZIOSpecDefault:

  def spec = suite("Interactive contract")(
    test("live illustration keys across the site are exactly the registry"):
      assertTrue(DocMounts.keys(BuildSite.pages*) == InteractiveRegistry.liveKeys)
    ,
    test("mount keys are unique across the whole site"):
      val all = DocMounts.keyList(BuildSite.pages*)
      assertTrue(all.nonEmpty, all.distinct.size == all.size),
  )
end InteractiveContractSpec
