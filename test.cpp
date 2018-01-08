#include <iostream>
#include <string>
#include <random>
#include <ctime>

int main()
{
  //rand(time(NULL));
  const int nrolls=10;  // number of experiments

  std::random_device rd;
  std::normal_distribution<float> dist(0.0,1.0);



  for (int i=0; i<nrolls; ++i) {
    float number = dist(rd);
    //if ((number>=0.0)&&(number<10.0)) ++p[int(number)];
    std::cout<<number<<std::endl;
  }

  return 0;
}
