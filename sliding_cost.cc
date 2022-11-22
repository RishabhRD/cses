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
  ll sleft = 0;
  ll sright = 0;
  auto const enforceConstraint = [&] {
    if (rightS.size() > leftS.size()) {
      auto const moving = *rightS.begin();
      sleft += moving;
      sright -= moving;
      leftS.insert(*rightS.begin());
      rightS.erase(rightS.begin());
    } else if (leftS.size() - rightS.size() > 1) {
      auto const moving = *leftS.rbegin();
      sright += moving;
      sleft -= moving;
      rightS.insert(*leftS.rbegin());
      leftS.erase(leftS.find(*leftS.rbegin()));
    }
  };

  auto const erase = [&](ll i) {
    if (leftS.find(i) != leftS.end()) {
      sleft -= i;
      leftS.erase(leftS.find(i));
    } else {
      sright -= i;
      rightS.erase(rightS.find(i));
    }
    enforceConstraint();
  };

  auto const insert = [&](ll i) {
    if (leftS.empty()) {
      leftS.insert(i);
      sleft += i;
    } else {
      auto const leftMax = *leftS.rbegin();
      if (i <= leftMax) {
        sleft += i;
        leftS.insert(i);
      } else {
        sright += i;
        rightS.insert(i);
      }
    }
    enforceConstraint();
  };

  std::vector<ll> res;
  ll low = 0;
  ll cur_cost = 0;
  auto const n = std::size(nums);
  for (ll high = 0; high < n; ++high) {
    insert(nums[high]);
    while (high - low + 1 > k) {
      auto const old_median = *leftS.rbegin();
      erase(nums[low]);
      cur_cost -= std::abs(old_median - nums[low]);
      ++low;
      auto const new_median = *leftS.rbegin();
      cur_cost += (high - low + 1) * std::abs(new_median - old_median);
    }
    if (high - low + 1 == k) {
      if (k % 2 == 0) {
        res.push_back(sright - sleft);
      } else {
        res.push_back(sright - sleft + (*leftS.rbegin()));
      }
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
