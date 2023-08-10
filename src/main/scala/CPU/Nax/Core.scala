package CPU.Nax
import spinal.core._
import CPU.Nax.utils._
import scala.collection.mutable._
import CPU.Nax.fetch._
import Lib.Sim.SpinalSim._
//generate core verilog here and set config here

class Core(val plugins: Seq[Plugin]) extends Component {
  val framework = new Framework(plugins)
}


object Config{
  def plugins(resetVector: BigInt = 0x80000000l,
              withRdTime: Boolean = true,
              ioRange: UInt => Bool = _(31 downto 28) === 0x1,
              fetchRange: UInt => Bool = _(31 downto 28) =/= 0x1,
              aluCount: Int = 2,
              decodeCount: Int = 2,
              withRvc: Boolean = false,
              withMmu: Boolean = true,
              withPerfCounters: Boolean = true,
              withSupervisor: Boolean = true,
              withDistributedRam: Boolean = true,
              xlen: Int = 32,
              withLoadStore: Boolean = true,
              withDedicatedLoadAgu: Boolean = false,
              withDebug: Boolean = false,
              withEmbeddedJtagTap: Boolean = false,
              withEmbeddedJtagInstruction: Boolean = false,
              jtagTunneled: Boolean = false,
              debugTriggers: Int = 0,
              branchCount: Int = 16,
              withFloat: Boolean = false,
              withDouble: Boolean = false,
              withLsu2: Boolean = true,
              lqSize: Int = 16,
              sqSize: Int = 16,
              simulation: Boolean = GenerationFlags.simulation,
              sideChannels: Boolean = false,
              dispatchSlots: Int = 32,
              robSize: Int = 64): ArrayBuffer[Plugin] = {
    val plugins = ArrayBuffer[Plugin]()

    plugins += new FetchPlugin()
    plugins += new PcPlugin(resetVector)

    plugins
  }
}

object Core extends App{
    //set some configs here
    val rtl = RtlConfig()
    rtl.setconfig(new Core(plugins = Config.plugins()))
}