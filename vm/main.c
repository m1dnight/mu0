#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int
file_size (FILE * f)
{
  fseek (f, 0L, SEEK_END);
  int sz = ftell (f);
  rewind (f);
  return sz;
}


void
main (int argc, char *argv[])
{
  // Read in the filename of the binary.
  char *filename = argv[1];
  printf ("file: %s\n", filename);

  // Read in the contents of the file.
  FILE *ptr;
  ptr = fopen (filename, "rb");
  int size = file_size (ptr);	// determine file size.
  unsigned char buffer[size];
  fread (buffer, sizeof (buffer), 1, ptr);

  // Print out the binary (debugging)
  for (int i = 0; i < size; i++)
    {
      printf ("0x%x  ", buffer[i]);
    }
  printf ("\n");

  // Setup the machine memory.
  unsigned char *memory = malloc (sizeof (char) * 32);
  for(int i = 0; i < 32; i++)
    {
      memory[i] = 0x0;
    }

  // Copy the program into memory.
  for (int i = 0; i < size; i++)
    {
      memory[i] = buffer[i];
    }

  // Internal state.
  unsigned char acc = 0x0;	// ACC register
  unsigned char PC = 0x0;	// program counter
  int halt = 0;			// halt flag

  // Start parsing the file.
  while (halt != 1)
    {
      unsigned char current = memory[PC];
      printf ("\n\n\nCurrent: 0x%x", current);
      printf("\n  ACC =  0x%x", acc);
      printf("\n  PC  =  0x%x", PC);      

      switch (current)
	{
	  unsigned char s;
	  unsigned char temp;

	  // ACC <= [S]
	case 0x0:
          printf("\nACC <= [S]");
          printf("\ns = 0x%x", memory[PC + 1]);
	  s = memory[PC + 1];
	  acc = memory[s];
	  PC++;
	  PC++;
	  break;

	  // ACC => [S]
	case 0x1:
          printf("\nACC => [S]");          
	  s = memory[PC + 1];
          printf("\n  Writing 0x%x to address 0x%x", acc, s);
	  memory[s] = acc;
	  PC++;
	  PC++;
	  break;

	  // ACC + [S]
	case 0x2:
          printf("\nACC + [S]");                    
	  s = memory[PC + 1];
	  temp = memory[s];
	  acc = acc + temp;
	  PC++;
	  PC++;
	  break;

	  // ACC - [S]
	case 0x3:
          printf("\nACC - [S]");                    
	  s = memory[PC + 1];
	  temp = memory[s];
	  acc = acc - temp;
	  PC++;
	  PC++;
	  break;


	  // PC <= S
	case 0x4:
          printf("\nPC <= S");                    
	  s = memory[PC + 1];
	  PC = s;
	  PC++;
	  PC++;
	  break;

	  // IF +VE PC <= S
	case 0x5:
          printf("\nIF +VE PC <= S");
	  s = memory[PC + 1];
	  if (acc > 0x0)
	    {
	      PC = s;
	    }

	  PC++;
	  PC++;
	  break;

	  // IF != 0 PC <= S
	case 0x6:
          printf("\nIF != 0 PC <= S");          
	  s = memory[PC + 1];
	  if (acc == 0x0)
	    {
	      PC = s;
	    }
	  PC++;
	  PC++;
	  break;

	case 0x7:
	  printf ("\nHALT");
	  halt = 1;
	  break;

	}
    }

  printf("Memory dump:\n");
  for(int i = 0; i < 32; i++)
    {
      printf("0x%x ", memory[i]);
    }      
}
