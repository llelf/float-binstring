#include <stdio.h>

int main (int argc, char* argv[])
{
  double x;
  if (argc == 3)
    {
      sscanf (argv[2], "%la", &x);
      printf ("%g\n", x);
    }
  else
    {
      sscanf (argv[1], "%lf", &x);
      printf ("%a\n", x);
    }
}

