import std.stdio;
import esdl.data.rand;

class Simple: Randomizable {
  mixin Randomization;
  @rand uint a;
  @rand uint b;
  Constraint! q{ a[0..5]  > 10; } csta;
}

void main()
{
  Simple simple = new Simple();
  for (size_t i=0; i!=4; ++i)
    {
      // simple.randomize();
      simple.randomise();
      writeln(simple.a, " ", simple.b);
    }
}
