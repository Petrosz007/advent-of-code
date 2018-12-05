#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

int main()
{
        ifstream infile("input-1-1.txt");
        int a;
        int sum = 0;
        while (infile >> a)
        {
                sum += a;
        }

        cout << sum << endl;

        return 0;
}