import esdl.data.cstx;

enum string CST=q{
  // ewfjewjofew
  // ewfoewfjew

  foreach(i, f; foo) {
    f < 64;
    f > 16;
  }
    
  foreach(j, g; bar) {
    g <= 16;
    g >= 0;
  }
};

void main()
{
  auto test = translate(CST);
  import std.stdio;
  writeln(test);
}
