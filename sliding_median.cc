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

auto solve(std::vector<ll> const &nums, ll k) {
  std::multiset<ll> leftS;
  std::multiset<ll> rightS;
  auto const enforceConstraint = [&] {
    if (rightS.size() > leftS.size()) {
      leftS.insert(*rightS.begin());
      rightS.erase(rightS.begin());
    } else if (leftS.size() - rightS.size() > 1) {
      rightS.insert(*leftS.rbegin());
      leftS.erase(leftS.find(*leftS.rbegin()));
    }
  };

  auto const erase = [&](ll i) {
    if (leftS.find(i) != leftS.end()) {
      leftS.erase(leftS.find(i));
    } else {
      rightS.erase(rightS.find(i));
    }
    enforceConstraint();
  };

  auto const insert = [&](ll i) {
    if (leftS.empty())
      leftS.insert(i);
    else {
      auto const leftMax = *leftS.rbegin();
      if (i <= leftMax) {
        leftS.insert(i);
      } else {
        rightS.insert(i);
      }
    }
    enforceConstraint();
  };

  std::vector<ll> res;
  ll low = 0;
  auto const n = std::size(nums);
  for (ll high = 0; high < n; ++high) {
    insert(nums[high]);
    while (high - low + 1 > k) {
      erase(nums[low]);
      ++low;
    }
    if (high - low + 1 == k) {
      res.push_back(*leftS.rbegin());
    }
  }
  return res;
}

int main() {
  auto const n = read<ll>();
  auto const k = read<ll>();
  auto const nums = read_vec<ll>(n);
  for (auto ele : solve(nums, k)) {
    std::cout << ele << ' ';
  }
  std::cout << std::endl;
}
