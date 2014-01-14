// Copyright: Coverify Systems Technology 2013 - 2014
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

import esdl.data.queue;

void main()
{
  import std.stdio;
  Queue!int q;

  for(int i=0; i < 4; ++i)
    {
      q.pushBack(i);
      q.pushFront(i);
    }

  q.insert(8, 98);
  q.insert(9, 99);
  writeln(q);
  q.remove(8);
  writeln(q);
  q.insert(0, 99);
  q.insert(1, 98);
  writeln(q);
  q.remove(1);
  writeln(q);
  
  foreach(ref int r; q)
    {
      writeln(r);
    }
    

  for(int i=0; i < 4; ++i)
    {
      q.removeFront();
      writeln(q);
      q.removeBack();
      writeln(q);
    }

  q = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
  writeln(q);

  import std.algorithm;
  auto r = filter!((int a){return a > 4;})(q[]);
  
  foreach(i; r)
    {
      writeln(i);
    }

  writeln(r.empty);

  int b = 12;

  auto f = countUntil!((int a){return a is b;})(q[]);
  
  writeln(f);

  
}

