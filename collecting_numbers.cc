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

class dsu {
private:
  using ll = long long;
  std::vector<ll> parent_;
  std::vector<ll> size_;

public:
  dsu(ll n) : parent_(n), size_(n, 1) {
    std::iota(std::begin(parent_), std::end(parent_), 0);
  }

  ll find(ll n) {
    if (parent_[n] == n)
      return n;
    auto ans = find(parent_[n]);
    parent_[n] = ans;
    return ans;
  }

  void combine(ll x, ll y) {
    auto const px = find(x);
    auto const py = find(y);
    if (px == py)
      return;
    if (size_[px] >= size_[py]) {
      size_[px] += size_[py];
      parent_[py] = px;
    } else {
      size_[py] += size_[px];
      parent_[px] = py;
    }
  }

  auto size(ll n) { return size_[find(n)]; }
};

auto solve(std::vector<int> const &vec) {
  auto const n = std::size(vec);
  dsu dsu(n + 1);
  std::unordered_set<int> mp;
  for (auto ele : vec) {
    if (mp.find(ele - 1) != mp.end()) {
      dsu.combine(ele, ele - 1);
    }
    mp.insert(ele);
  }
  std::unordered_set<int> st;
  for (int i = 1; i <= n; ++i) {
    st.insert(dsu.find(i));
  }
  return st.size();
}

int main() {
  auto const n = read<int>();
  auto const vec = read_vec<int>(n);
  std::cout << solve(vec) << std::endl;
}
