package heddle.server

import neotype.*

/** A TCP port. `0` asks the operating system for one. */
type Port = Port.Type
object Port extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 0 && input <= 65535 then true
    else s"a port must be 0 to 65535, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end Port

/** How many HTTP/1.1 requests one connection may serve before `Connection: close`. */
type KeepAliveRequests = KeepAliveRequests.Type
object KeepAliveRequests extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input > 0 then true
    else s"keep-alive requests must be a positive int, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end KeepAliveRequests

/** Accepted sockets this process will hold. */
type MaxConnections = MaxConnections.Type
object MaxConnections extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input > 0 then true
    else s"max connections must be a positive int, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end MaxConnections

/** Listen backlog. `0` leaves the depth to the operating system. */
type Backlog = Backlog.Type
object Backlog extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 0 then true
    else s"a listen backlog must be >= 0, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end Backlog

/** Bytes in one read or write. Positive, because a buffer is allocated from it. */
type ChunkSize = ChunkSize.Type
object ChunkSize extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input > 0 then true
    else s"chunk size must be a positive int, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end ChunkSize

/** Bytes of one header block. Positive, and small enough to allocate. */
type HeaderBytes = HeaderBytes.Type
object HeaderBytes extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input > 0 then true
    else s"header bytes must be a positive int, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end HeaderBytes

/** A body cap. `0` refuses every body. The value may be larger than an `Int`. */
type BodyBytes = BodyBytes.Type
object BodyBytes extends Newtype[Long]:
  override inline def validate(input: Long): Boolean | String =
    if input >= 0 then true
    else s"body bytes must be >= 0, got $input"

  extension (value: Type) def toLong: Long = value.unwrap
end BodyBytes

/** HTTP/2 `SETTINGS_MAX_CONCURRENT_STREAMS`. `0` refuses new streams. */
type ConcurrentStreams = ConcurrentStreams.Type
object ConcurrentStreams extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 0 then true
    else s"concurrent streams must be >= 0, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end ConcurrentStreams

/** HTTP/2 flow-control window. `0` is legal and stalls the sender until a window update. */
type WindowSize = WindowSize.Type
object WindowSize extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 0 then true
    else s"a window size must be >= 0, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end WindowSize

/** `SETTINGS_MAX_FRAME_SIZE`, RFC 9113 §6.5.2: 16,384 through 16,777,215. */
type FrameSize = FrameSize.Type
object FrameSize extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 16384 && input <= 16777215 then true
    else s"a frame size must be 16384 to 16777215, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end FrameSize

/** HTTP/2 `SETTINGS_MAX_HEADER_LIST_SIZE`. `0` means the peer advertised no list. */
type HeaderListSize = HeaderListSize.Type
object HeaderListSize extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 0 then true
    else s"a header list size must be >= 0, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end HeaderListSize

/** Frames waiting on one HTTP/2 connection, writer queue and body queue alike. */
type OutstandingFrames = OutstandingFrames.Type
object OutstandingFrames extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input > 0 then true
    else s"outstanding frames must be a positive int, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
end OutstandingFrames
