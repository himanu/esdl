import std.stdio;
import esdl.data.rand;

class Base: Randomizable {
  mixin Randomization;
}

class Simple: Base {
  mixin Randomization;
  @rand uint a;
  Constraint! q{ a < 10; } csta;
}

void main()
{
  Simple simple = new Simple();
  for (size_t i=0; i!=4; ++i)
    {
      simple.randomize();
      writeln(simple.a);
    }
}
