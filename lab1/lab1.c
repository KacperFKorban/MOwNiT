#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>

int main (void) {
    float x = 1.1;
    int i = 0;
    while(i < 149) {
        printf("\n");
        gsl_ieee_printf_float(&x);
        x = x / 2.0f;
        i++;
    }

    /*
        LD_LIBRARY_PATH=/home/kacper/gsl/ <---- should be path to gsl installation folder
        export LD_LIBRARY_PATH

        gcc -Wall -I/home/kacper/gsl/include -c lab1.c
        gcc -L/home/kacper/gsl/lib lab1.o -lgsl -lgslcblas -lm -o lab1
        ./lab1
    */

    return 0;
}