#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct {
    double temp;
    double dens;
    double pres;
} AtmState; 

const double g = 9.80665;

AtmState calculate(double target_alt);
void increment(AtmState* state, double alt_increase, double temp_grad);
void increment_iso(AtmState* state, double alt_increase);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Invalid arguments");
    } else {
        double target_alt = atof(argv[1]); // in meters

        AtmState result = calculate(target_alt);

        printf("Temperature: %f K\n", result.temp);
        printf("Pressure: %f Pa\n", result.pres);
        printf("Density: %f kg/m^3\n\n", result.dens);
    }
}

AtmState calculate(double target_alt) {
    AtmState state;

    state.temp = 288.15;
    state.pres = 101325;

    if (target_alt < 11000.) {
        increment(&state, target_alt, -0.0065);
    } else {
        increment(&state, 11000., -0.0065);

        if (target_alt < 20000.) {
            increment_iso(&state, target_alt - 11000.);
        } else {
            increment_iso(&state, 9000.);

            if (target_alt < 32000.) {
                increment(&state, target_alt - 20000., 0.001);
            } else {
                increment(&state, 12000., 0.001);

                if (target_alt < 47000.) {
                    increment(&state, target_alt - 32000., 0.0028);
                } else {
                    increment(&state, 15000., 0.0028);

                    increment_iso(&state, target_alt - 47000.);
                }
            }
        }
    }


    state.dens = state.pres / (287. * state.temp);

    return state;
}

void increment(AtmState* state, double alt_increase, double temp_grad) {
    double prev_temp = state->temp;
    state->temp = prev_temp + temp_grad * alt_increase;
    state->pres = state->pres * pow(state->temp / prev_temp, -g/(temp_grad * 287));
}

void increment_iso(AtmState* state, double alt_increase) {
    state->pres = state->pres * exp(-g * alt_increase/(287 * state->temp));
}
