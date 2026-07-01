package dfhdl.ips.interactive
import dfhdl.*
import compiler.ir
import internals.{AssertGiven, metaContextIgnore}
import core.{DFC, DFDclAny, connect, DFVal, Modifier, DFTypeAny}

extension [C, M <: Modifier[Any, C, Any, Any]](dcl: DFVal[DFTypeAny, M])
  @metaContextIgnore
  def interactiveCtrl(using dfc: DFC)(using ConnectableOnly[C]): Unit =
    import dfc.getSet
    val dclIR = dcl.asIR
    val name = dclIR.getName
    val width = dclIR.dfType.widthUNSAFE
    @metaContextIgnore def wrap(using DFC) =
      interactive_ctrl(NAME = name, WIDTH = width)
    val ctrl = wrap(using dfc.setName(s"${name}_interactive_ctrl"))
    val ctrlValue = dclIR.dfType match
      case ir.DFBits(_) => ctrl.value
      case _            => ctrl.value.as(dcl.dfType)(using dfc.anonymize)
    dcl.connect(ctrlValue)

  @metaContextIgnore
  def interactiveFlag(using dfc: DFC)(using ConnectableOnly[C]): Unit =
    import dfc.getSet
    val dclIR = dcl.asIR
    val name = dclIR.getName
    val width = dclIR.dfType.widthUNSAFE
    @metaContextIgnore def wrap(using DFC) =
      interactive_flag(NAME = name, WIDTH = width)
    val flag = wrap(using dfc.setName(s"${name}_interactive_flag"))
    val dclBits = dclIR.dfType match
      case ir.DFBits(_) => dcl
      case _            => dcl.bits(using dfc.anonymize)
    flag.value.connect(dclBits)
end extension

private type ConnectableOnly[C] = AssertGiven[
  C =:= Modifier.Connectable,
  "Only a port or variable can be connected to an interactive control or flag."
]
