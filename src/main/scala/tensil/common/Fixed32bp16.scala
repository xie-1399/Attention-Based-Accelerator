package tensil.common

/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

object Fixed32bp16
  extends FixedBase(
    width = 32,
    basePoint = 16,
    sizeBytes = 4,
    fromLongBits = (bits: Long) => new Fixed32bp16(bits.toInt),
    toLongBits = (fixed: Fixed32bp16) => fixed.bits.toLong
  ) {

  implicit val numericWithMAC = mkNumericWithMAC
}

class Fixed32bp16(
                   val bits: Int
                 ) extends AnyVal {}
