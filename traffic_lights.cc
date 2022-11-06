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

auto solve(int x, std::vector<int> const &queries) {
  std::set<int> r_pos({0, x});
  std::set<int, std::greater<int>> l_pos({0, x});
  std::map<int, int, std::greater<int>> mp;
  ++mp[x];
  for (auto pos : queries) {
    auto const left = *l_pos.upper_bound(pos);
    auto const right = *r_pos.upper_bound(pos);
    --mp[right - left];
    if (mp[right - left] == 0)
      mp.erase(right - left);
    ++mp[pos - left];
    ++mp[right - pos];
    l_pos.insert(pos);
    r_pos.insert(pos);
    std::cout << mp.begin()->first << std::endl;
  }
}

int main() {
  auto const x = read<int>();
  auto const n = read<int>();
  auto const nums = read_vec<int>(n);
  solve(x, nums);
}
