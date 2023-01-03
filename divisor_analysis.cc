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

ll power(ll n, ll x) {
  if (x == 0)
    return 1;
  auto ans = power(n, x / 2);
  ans *= ans;
  if (x % 2)
    ans *= n;
  return ans;
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

ll calc_numbers(std::vector<std::pair<ll, ll>> const &factors, ll mod) {
  ll ans = 1;
  for (auto const [_, alpha] : factors) {
    ans = (ans * ((alpha + 1) % mod)) % mod;
  }
  return ans;
}

ll calc_sum(std::vector<std::pair<ll, ll>> const &factors) {
  ll ans = 1;
  for (auto const [n, alpha] : factors) {
    ll res = ((mod_power(n, alpha + 1, mod) - 1) * inv(n - 1, mod)) % mod;
    ans = (ans * res) % mod;
  }
  return ans;
}

ll calc_product(std::vector<std::pair<ll, ll>> const &factors) {
  ll num = 1;
  ll t = 1;
  bool done = false;
  for (auto [n, alpha] : factors) {
    ll m = (alpha + 1) % (mod - 1);
    if ((alpha + 1) % 2 == 0) {
      if (!done) {
        done = true;
        m /= 2;
      }
    }
    t = (t * m) % (mod - 1);
  }
  for (auto [n, alpha] : factors) {
    ll power = alpha;
    if (!done)
      power = (alpha * inv(2, mod)) % mod;
    num = (num * mod_power(n, power, mod)) % mod;
  }
  return mod_power(num, t, mod);
}

auto solve(std::vector<std::pair<ll, ll>> const &factors) {
  auto const numbers = calc_numbers(factors, mod);
  auto const sum = calc_sum(factors);
  auto const product = calc_product(factors);
  std::cout << numbers << ' ' << sum << ' ' << product << std::endl;
}

int main() {
  auto const n = read<ll>();
  std::vector<std::pair<ll, ll>> factors(n);
  for (auto &[a, b] : factors)
    std::cin >> a >> b;
  solve(factors);
}
