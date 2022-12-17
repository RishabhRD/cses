#include <algorithm>
#include <array>
#include <bitset>
#include <chrono>
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

struct custom_hash {
  static uint64_t splitmix64(uint64_t x) {
    x += 0x9e3779b97f4a7c15;
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
    x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
    return x ^ (x >> 31);
  }

  size_t operator()(uint64_t x) const {
    static const uint64_t FIXED_RANDOM =
        std::chrono::steady_clock::now().time_since_epoch().count();
    return splitmix64(x + FIXED_RANDOM);
  }
};

using ll = long long;
constexpr ll mod = 1e9 + 7;

using safe_set = std::unordered_set<ll, custom_hash>;

template <typename T> using safe_map = std::unordered_map<ll, T>;

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

template <typename T> auto read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

class dsu {
private:
public:
  std::vector<ll> parent_;
  std::vector<ll> size_;

  dsu(ll n) : parent_(n), size_(n, 1) {
    std::iota(std::begin(parent_), std::end(parent_), 0);
  }

  ll find(ll n) {
    if (parent_[n] == n)
      return n;
    return parent_[n] = find(parent_[n]);
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

auto solve(ll n, std::vector<std::tuple<ll, ll, ll>> &edges) {
  dsu dsu(n + 1);
  std::sort(std::begin(edges), std::end(edges),
            [](auto e1, auto e2) { return std::get<2>(e1) < std::get<2>(e2); });
  ll sum = 0;
  for (auto [x, y, w] : edges) {
    if (dsu.find(x) == dsu.find(y))
      continue;
    dsu.combine(x, y);
    sum += w;
  }
  std::unordered_set<ll> st;
  for (ll i = 1; i <= n; ++i)
    st.insert(dsu.find(i));
  if (st.size() > 1)
    std::cout << "IMPOSSIBLE" << std::endl;
  else {
    std::cout << sum << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::vector<std::tuple<ll, ll, ll>> edges(m);
  for (auto &[x, y, w] : edges)
    std::cin >> x >> y >> w;
  solve(n, edges);
}
