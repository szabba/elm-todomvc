package com.github.szabba.todomvc.replicated.crdt

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class NodeID(rawID: String)

object NodeID {
  implicit val ordering: Ordering[NodeID] =
    Ordering.by(_.rawID)

  implicit val encoder: Encoder[NodeID] =
    Encoder.encodeString.contramap(_.rawID)

  implicit val decoder: Decoder[NodeID] =
    Decoder.decodeString.map(apply)

  implicit val keyEncoder: KeyEncoder[NodeID] =
    KeyEncoder.encodeKeyString.contramap(_.rawID)

  implicit val keyDecoder: KeyDecoder[NodeID] =
    KeyDecoder.decodeKeyString.map(apply)
}
