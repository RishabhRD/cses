#include <algorithm>
#include <array>
#include <bitset>
#include <cmath>
#include <deque>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <numeric>
#include <optional>
#include <queue>
#include <set>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

template <typename T> T read() {
  T t;
  std::cin >> t;
  return t;
}

template <typename T> std::vector<T> read_vec(int n) {
  std::vector<T> vec(n);
  for (auto &ele : vec)
    std::cin >> ele;
  return vec;
}

template <typename T> std::vector<T> read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

using ll = long long;

auto solve(std::vector<int> &res) {
  std::sort(std::begin(res), std::end(res),
            [](auto a, auto b) { return std::abs(a) < std::abs(b); });
  int cur = 0;
  int max = 0;
  for (auto ele : res) {
    if (ele >= 0)
      cur += 1;
    else
      cur -= 1;
    max = std::max(max, cur);
  }
  return max;
}

int main() {
  auto const n = read<int>();
  std::vector<int> res;
  for (int i = 0; i < n; ++i) {
    res.push_back(read<int>());
    res.push_back(-read<int>());
  }
  std::cout << solve(res) << std::endl;
}
