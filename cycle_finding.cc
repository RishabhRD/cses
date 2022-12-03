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

constexpr ll inf = 1e14;

auto solve(std::vector<std::tuple<ll, ll, ll>> edges, ll n) {
  std::vector dist(n + 1, ll(inf));
  dist[1] = 0;
  std::vector<ll> parent(n + 1, -1);
  for (ll i = 0; i < n; ++i) {
    for (auto const [u, v, w] : edges) {
      if (dist[v] > dist[u] + w) {
        parent[v] = u;
        dist[v] = dist[u] + w;
      }
    }
  }
  ll till = -1;
  for (auto const [u, v, w] : edges) {
    if (dist[v] > dist[u] + w) {
      // this is possible that this v is not part of cycle itself
      // and it is being relaxed due to impact of negative cycle
      // as it was connected forward to cycle
      parent[v] = u;
      till = v;
    }
  }
  if (till == -1) {
    std::cout << "NO" << std::endl;
  } else {
    std::cout << "YES" << std::endl;
    std::vector<ll> cycles;
    // make sure that we are in the cycle as till can be out of cycle
    for (ll i = 0; i < n; ++i)
      till = parent[till];
    ll cur = till;
    do {
      cycles.push_back(cur);
      cur = parent[cur];
    } while (cur != till);
    std::reverse(std::begin(cycles), std::end(cycles));
    for (auto ele : cycles)
      std::cout << ele << ' ';
    std::cout << cycles[0] << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::vector<std::tuple<ll, ll, ll>> edges;
  for (ll i = 0; i < m; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    auto const w = read<ll>();
    edges.push_back({a, b, w});
  }
  solve(edges, n);
}
