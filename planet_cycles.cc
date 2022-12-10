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

auto get_cycle_idx(std::vector<ll> const &to, ll i,
                   std::vector<ll> const &path_len) {
  ll slow = i;
  ll fast = i;
  do {
    if (path_len[slow] != -1)
      return slow;
    slow = to[slow];
    fast = to[to[fast]];
  } while (slow != fast);
  return slow;
}

auto get_cycle_ele(std::vector<ll> const &to, ll i) {
  std::unordered_set<ll> visited;
  while (visited.find(i) == visited.end()) {
    visited.insert(i);
    i = to[i];
  }
  return visited;
}

ll get_path_len(std::vector<ll> const &to, ll i, std::vector<ll> &path_len) {
  if (path_len[i] != -1)
    return path_len[i];
  return path_len[i] = 1 + get_path_len(to, to[i], path_len);
}

auto solve(std::vector<ll> const &to) {
  ll cycle_id = 0;
  auto const n = std::size(to);
  std::vector<ll> path_len(n, -1);
  for (ll i = 1; i < n; ++i) {
    if (path_len[i] != -1)
      continue;
    auto const cycle_idx = get_cycle_idx(to, i, path_len);
    if (path_len[cycle_idx] == -1) {
      auto const cycle_ele = get_cycle_ele(to, cycle_idx);
      for (auto const e : cycle_ele) {
        path_len[e] = cycle_ele.size();
      }
    }
    get_path_len(to, i, path_len);
  }
  for (ll i = 1; i < n; ++i)
    std::cout << path_len[i] << ' ';
  std::cout << std::endl;
}

int main() {
  auto const n = read<ll>();
  std::vector<ll> to(n + 1);
  for (ll i = 1; i <= n; ++i)
    std::cin >> to[i];
  solve(to);
}
