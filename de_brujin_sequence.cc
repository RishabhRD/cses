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

using graph_t = std::vector<std::unordered_set<ll>>;

auto generate_graph(ll n) {
  auto const sz = 1 << n;
  graph_t graph(sz);
  for (ll i = 0; i < sz; ++i) {
    auto const first = (i & ((1 << (n - 1)) - 1)) << 1;
    auto const second = first + 1;
    graph[i].insert(first);
    graph[i].insert(second);
  }
  return graph;
}

void make_euler(ll i, graph_t &graph, std::vector<ll> &ans) {
  while (not std::empty(graph[i])) {
    auto const n = *graph[i].begin();
    graph[i].erase(n);
    make_euler(n, graph, ans);
  }
  ans.push_back(i);
}

auto solve(ll n) {
  if (n == 1) {
    std::cout << "01" << std::endl;
    return;
  }
  auto graph = generate_graph(n - 1);
  std::vector<ll> ans;
  make_euler(0, graph, ans);
  std::reverse(std::begin(ans), std::end(ans));
  std::string res(n - 1, '0');
  for (ll i = 1; i < ans.size(); ++i) {
    if (ans[i] % 2)
      res.push_back('1');
    else
      res.push_back('0');
  }
  std::cout << res << std::endl;
}

int main() {
  auto const n = read<ll>();
  solve(n);
}
