import std.stdio;
import esdl.data.rand;

class Base {
  @rand uint x;
  mixin Randomization;
  Constraint! q{x < 28;} cstx;
}

class Simple: Base {
  mixin Randomization;
  @rand uint a;
  Constraint! q{
    a[0..4]  == 0;
    a < 128;
  } csta;
}

void main()
{
  Simple simple = new Simple();
  for (size_t i=0; i!=4; ++i)
    {
      // simple.randomize();
      simple.randomize();
      writeln(simple.a, " ", simple.x);
    }
}
