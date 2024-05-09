#include <iostream>
#include "kernel.cpp"

int main(){
    population population(1, true, 12000, false);
    population.appendPopulation(3, true, 12000, true);

    std::cout << population.getReserves() << std::endl;

    system("pause");
    return 0;
}