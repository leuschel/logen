#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define TEMPLATE_LEN 256
int main(int argc, char **argv)
{
	char template[TEMPLATE_LEN];
	char *dir;

	if(argc == 1)
	{
		strcpy(template, "/tmp/tempdirXXXXXX");
	}
	else
	{
		if(strlen(argv[1]) > TEMPLATE_LEN -6 -1)
		{
			fprintf(stderr, "Template cannot be larger than %d characters.\n",
					TEMPLATE_LEN - 6 - 1);
			return EXIT_FAILURE;
		}
		strcpy(template, argv[1]);
		strcpy(template + strlen(argv[1]), "XXXXXX");
	}
	/*printf("Template is %s\n", template);*/
	

	dir = mkdtemp(template);
	if(!dir)
	{
		perror("tempdir: could not create temporary directory");
		exit(1);
	}

	printf("Directory created : %s\n", dir);
	return EXIT_SUCCESS;
}
