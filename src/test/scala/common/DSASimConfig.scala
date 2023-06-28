package common

import spinal.core.SpinalConfig
import spinal.core.sim.SimConfig

object DSASimConfig {
  def apply() = SimConfig.withVcdWave.withConfig(
    SpinalConfig(targetDirectory = "rtl")).workspacePath("simulation")
}
