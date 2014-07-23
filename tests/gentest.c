#include <stdio.h>
#include <stdlib.h>


double random_value ()
{
  double x;
  arc4random_buf (&x, sizeof x);
  return x;
}


int main ()
{
  FILE *out = fopen ("tests.pairs.10k", "w");

  for (int i = 0; i < 10*1024; i++)
    {
      double x = random_value();
      fprintf (out, "%.20g\t%a\n", x, x);
    }
}
