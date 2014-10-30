import esdl.vcd.parse;

void main(string[] args)
{
  VCD vcd = new VCD(args[1]);
  auto node = vcd.find("basic_avl_tb_top.dut.clk_ref[0]");
  assert(node !is null);
  VcdVecWave!8 wave = cast(VcdVecWave!8) node;
  import std.stdio;
  writeln(wave._wave);
}
