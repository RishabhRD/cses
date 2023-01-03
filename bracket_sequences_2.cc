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

ll mod_power(ll n, ll x, ll mod) {
  if (x == 0)
    return 1;
  auto ans = mod_power(n, x / 2, mod);
  ans = (ans * ans) % mod;
  if (x % 2)
    ans = (ans * n) % mod;
  return ans;
}

ll inv(ll n, ll mod) { return mod_power(n, mod - 2, mod); }

ll solve(ll n, ll k) {
  std::vector<ll> factorial(2 * n + k + 1);
  factorial[0] = 1;
  for (ll i = 1; i <= 2 * n + k; ++i) {
    factorial[i] = (factorial[i - 1] * i) % mod;
  }
  ll ans = factorial[2 * n + k];
  ans = (ans * (k + 1)) % mod;
  ans = (ans * inv(factorial[n], mod)) % mod;
  ans = (ans * inv(factorial[2 * n + k - n], mod)) % mod;
  ans = (ans * inv(n + k + 1, mod)) % mod;
  return ans;
}

int main() {
  auto const N = read<ll>();
  if (N % 2) {
    std::cout << 0 << std::endl;
    return 0;
  }
  auto const str = read<std::string>();
  ll k = 0;
  for (auto c : str) {
    if (c == '(')
      ++k;
    if (c == ')')
      --k;
    if (k < 0) {
      std::cout << 0 << std::endl;
      return 0;
    }
  }
  auto const n = (N - str.size() - k) / 2;
  std::cout << solve(n, k) << std::endl;
}
