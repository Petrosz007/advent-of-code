#include <iostream>
#include <fstream>
#include <map>

using namespace std;

int main() {
    ifstream file;
    int szam, osszeg = 0;
    map<int, bool> szamok;
    while (true)    {
        file.open("input-1-1.txt", ios::in);
        while (!file.eof()) {
            file >> szam;
            osszeg += szam;
            if (szamok[osszeg]) {
                //file.close();
                cout << osszeg;
                exit(0);
            }
            szamok[osszeg] = true;
        }
        file.close();
    }

    return 0;
}