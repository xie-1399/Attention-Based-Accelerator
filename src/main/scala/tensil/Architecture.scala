package tensil

/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */


case class Architecture(
                         dataType:String,  //Todo with the data type
                         arraySize: Int,
                         dram0Depth: Long,
                         dram1Depth: Long,
                         localDepth: Long,
                         accumulatorDepth: Long,
                         simdRegistersDepth: Int,
                         stride0Depth: Int,
                         stride1Depth: Int,
                         numberOfThreads: Int,
                         threadQueueDepth: Int,
                       ) {
}
