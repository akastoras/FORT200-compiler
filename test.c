#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

int main()
{
	char str[] = "123.456";
	char *ptr;
	long long int a, b;

	//Extract the whole number part
	ptr = strtok(str, ".");
	a = strtoll(ptr, NULL, 16);

	//Extract the fractional part
	ptr = strtok(NULL, ".");
	b = strtoll(ptr, NULL, 16);

	//Calculate the decimal number
	// double result = a + ((double) b/pow(16,strlen(ptr)));

	printf("The decimal number is %f\n", a + ((double) b/pow(16,strlen(ptr))));
	return 0;
}