#include <iostream>
#include <vector>
#include <fstream>
#include <string>
using namespace std;

using vd = vector<double>;
using vvd = vector<vd>;

void read(const string& filename, vvd& v) {
  int i = 0;
  v.push_back(vd(0));
  ifstream f(filename);
  double d;
  int cont = 0;
  while (f >> d) {
    v[i].push_back(d);
    cont++;
    if (cont == 75){
      string aux; f >> aux;
      cont = 0;
      ++i;
      v.push_back(vd(0));
    }
  }
  f.close();
}

void write(string& filename, const vvd& v){
  string aux;
  for (int i = 0; i < filename.size()-4; i++) aux.push_back(filename[i]);
  filename = aux + "csv";
  ofstream f(filename);
  for (const vd& x : v){
    for (int i = 0; i < x.size(); i++){
      if (i == 0) f << x[0];
      else f << "," << x[i];
    }
    f << endl;
  }
  f.close();
}

int main(){
  cout << "filename: " << endl;
  string filename;
  cin >> filename;
  vvd v;
  read(filename, v);
  write(filename, v);
}
