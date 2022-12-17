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

struct fenwick_tree_2d {
  std::vector<std::vector<ll>> bit;

  fenwick_tree_2d(std::vector<std::vector<ll>> const &a)
      : bit(a.size(), std::vector<ll>(a[0].size())) {
    auto const m = a.size();
    auto const n = a.size();
    for (ll i = 0; i < m; ++i) {
      for (ll j = 0; j < n; ++j) {
        add(i, j, a[i][j]);
      }
    }
  }

  ll sum(ll x, ll y) {
    ll ret = 0;
    for (ll i = x; i >= 0; i = (i & (i + 1)) - 1)
      for (ll j = y; j >= 0; j = (j & (j + 1)) - 1)
        ret += bit[i][j];
    return ret;
  }

  void add(ll x, ll y, char value) {
    auto const m = bit.size();
    auto const n = bit[0].size();
    for (ll i = x; i < m; i = i | (i + 1))
      for (ll j = y; j < n; j = j | (j + 1))
        bit[i][j] += value;
  }
};

auto solve(std::vector<std::string> const &arr_,
           std::vector<std::vector<ll>> const &queries) {
  auto const m = std::size(arr_);
  auto const n = std::size(arr_);
  std::vector arr(m, std::vector(n, 0ll));
  for (ll i = 0; i < m; ++i) {
    for (ll j = 0; j < n; ++j) {
      if (arr_[i][j] == '.')
        arr[i][j] = 0;
      else
        arr[i][j] = 1;
    }
  }
  fenwick_tree_2d ftree(arr);
  for (auto const &query : queries) {
    auto const q = query[0];
    if (q == 1) {
      auto const x = query[1];
      auto const y = query[2];
      ll const to_add = arr[x][y] == 1 ? -1 : 1;
      arr[x][y] += to_add;
      ftree.add(x, y, to_add);
    } else {
      auto const x1 = query[1];
      auto const y1 = query[2];
      auto const x2 = query[3];
      auto const y2 = query[4];
      auto const d = ftree.sum(x2, y2);
      auto const b = x1 != 0 ? ftree.sum(x1 - 1, y2) : 0;
      auto const c = y1 != 0 ? ftree.sum(x2, y1 - 1) : 0;
      auto const a = (x1 != 0 && y1 != 0) ? ftree.sum(x1 - 1, y1 - 1) : 0;
      std::cout << d - b - c + a << std::endl;
    }
  }
}

int main() {
  auto const n = read<ll>();
  auto const q = read<ll>();
  std::vector<std::string> matrix(n);
  for (auto &str : matrix)
    std::cin >> str;
  std::vector<std::vector<ll>> queries;
  for (ll i = 0; i < q; ++i) {
    auto const q = read<ll>();
    if (q == 1) {
      auto const x = read<ll>() - 1;
      auto const y = read<ll>() - 1;
      queries.push_back(std::vector<ll>({q, x, y}));
    } else {
      auto const x1 = read<ll>() - 1;
      auto const y1 = read<ll>() - 1;
      auto const x2 = read<ll>() - 1;
      auto const y2 = read<ll>() - 1;
      queries.push_back(std::vector<ll>({q, x1, y1, x2, y2}));
    }
  }
  solve(matrix, queries);
}
