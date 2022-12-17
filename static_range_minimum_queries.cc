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

auto calc_log2(ll n) { return __builtin_clzll(1) - __builtin_clzll(n); }

template <typename T, typename AssociativeFunc>
auto range_query_idempotent(std::vector<T> const &nums,
                            AssociativeFunc &&func) {
  constexpr static auto const k = 25;
  auto const n = std::size(nums);
  std::vector rng(k + 1, std::vector<T>(n));
  rng[0] = nums;
  for (ll i = 1; i <= k; ++i) {
    auto const len = 1 << i;
    auto const prev_len = 1 << (i - 1);
    for (ll j = 0; j + len - 1 < n; ++j) {
      rng[i][j] = func(rng[i - 1][j], rng[i - 1][j + prev_len]);
    }
  }
  return [rng = std::move(rng),
          func = std::forward<AssociativeFunc>(func)](ll a, ll b) {
    auto const len = (b - a + 1);
    auto const i = calc_log2(len);
    return func(rng[i][a], rng[i][b - (1 << i) + 1]);
  };
}

auto solve(std::vector<ll> const &nums,
           std::vector<std::pair<ll, ll>> const &queries) {
  auto rng = range_query_idempotent(
      nums, [](auto a, auto b) { return std::min(a, b); });
  for (auto [a, b] : queries) {
    std::cout << rng(a - 1, b - 1) << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const q = read<ll>();
  auto const nums = read_vec<ll>(n);
  std::vector<std::pair<ll, ll>> queries(q);
  for (auto &[a, b] : queries) {
    std::cin >> a >> b;
  }
  solve(nums, queries);
}
