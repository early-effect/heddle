package heddle.docs

import specular.*
import zio.test.*

/** JVM half of the mount contract: every `exampleDom` key the site declares is one ClientMain binds. */
object InteractiveContractSpec extends ZIOSpecDefault:

  def spec = suite("Interactive contract")(
    test("exampleDom keys across the site are exactly the ones ClientMain binds"):
      assertTrue(DocMounts.domKeys(BuildSite.pages*) == InteractiveRegistry.domKeys)
    ,
    test("mount keys are unique across the whole site"):
      val all = DocMounts.keyList(BuildSite.pages*)
      assertTrue(all.nonEmpty, all.distinct.size == all.size)
    ,
    test("every exampleDom source resolves against the source root"):
      val results = DocMounts
        .domExamples(BuildSite.pages*)
        .map(d => d.source.describe -> DomSourceLoader.resolve(d.source, DomSourceLoader.sourceRoot))
      assertTrue(results.nonEmpty, results.forall(_._2.isRight)),
  )
end InteractiveContractSpec
