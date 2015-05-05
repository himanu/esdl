import esdl.data.rand;

class Foo {
  mixin Randomization;
  @rand int frop;
  Constraint!q{
    frop >= 0;
    frop < 128;
  } frop_cst;
public:
  void display() {
    import std.stdio;
    writeln("frop is: ", frop);
  }
  this() {
  };
}

class Bar {
  mixin Randomization;
  @rand Foo foo;
  this() {
    foo = new Foo();
  }
  void preRandomize() {
    foo = new Foo();
  }
  Constraint!q{
    foo.frop >= 64;
  } frop_cst;
public:
  void display() {
    foo.display();
  }
}

void main()
{
  Bar bar = new Bar();
  for (size_t i=0; i!=4; ++i)
    {
      bar.randomize();
      bar.display();
    }
}
