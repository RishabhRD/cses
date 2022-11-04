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

auto solve(std::multiset<int, std::greater<int>> &st,
           std::vector<int> const &max_price) {
  for (auto const ele : max_price) {
    auto itr = st.lower_bound(ele);
    if (itr == st.end()) {
      std::cout << -1 << std::endl;
    } else {
      std::cout << *itr << std::endl;
      st.erase(itr);
    }
  }
}

int main() {
  auto const n = read<int>();
  auto const m = read<int>();
  auto const tickets = read_vec<int>(n);
  auto const max_price = read_vec<int>(m);
  std::multiset<int, std::greater<int>> st;
  for (auto ele : tickets)
    st.insert(ele);
  solve(st, max_price);
}
