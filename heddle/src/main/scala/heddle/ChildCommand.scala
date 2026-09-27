package heddle

import zio.Chunk

/** A program to start as a child process. Arguments are passed as they are: no shell ever interprets them. `env` adds
  * to the parent's environment.
  */
final case class ChildCommand(
    program: String,
    args: Chunk[String] = Chunk.empty,
    env: Map[String, String] = Map.empty,
    cwd: Option[String] = None,
):
  def render: String = (program +: args).mkString(" ")

/** The child could not be started: no such program, no permission, a bad directory. */
final case class SpawnFailed(command: ChildCommand, cause: Throwable)
