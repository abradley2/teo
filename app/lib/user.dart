import 'package:flutter_riverpod/flutter_riverpod.dart';

final userProvider = StateNotifierProvider<User, UserData>((ref) {
  return User(UserData(null));
});

class UserData {
  String? userId;

  UserData(this.userId);
}

class User extends StateNotifier<UserData> {
  User(UserData userData) : super(userData);

  void login(String userId) {
    state = UserData(userId);
  }

  void logout() {
    state = UserData(null);
  }
}
