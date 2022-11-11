import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'user.dart';

final loginFormProvider = StateNotifierProvider<LoginFormNotifier, LoginForm>(
  (ref) => LoginFormNotifier(LoginForm()),
);

class LoginFormNotifier extends StateNotifier<LoginForm> {
  LoginFormNotifier(LoginForm state) : super(state);

  void setUserId(String userId) {
    state = state.copyWith(userId: userId);
  }

  void setPassword(String password) {
    state = state.copyWith(password: password);
  }
}

class LoginForm {
  String? userId;
  String? password;
  LoginForm({this.userId, this.password});

  LoginForm copyWith({String? userId, String? password}) {
    return LoginForm(
      userId: userId ?? this.userId,
      password: password ?? this.password,
    );
  }
}

void main() {
  runApp(const MyApp());
}

class LoginPage extends ConsumerWidget {
  const LoginPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final user = ref.watch(userProvider);
    final loginForm = ref.watch(loginFormProvider);
    return Scaffold(
        appBar: AppBar(
          // Here we take the value from the MyHomePage object that was created by
          // the App.build method, and use it to set our appbar title.
          title: const Text("Login"),
        ),
        body: Row(mainAxisAlignment: MainAxisAlignment.center, children: [
          Column(mainAxisAlignment: MainAxisAlignment.center, children: [
            SizedBox(
                width: 200,
                child: TextField(onChanged: (String nextValue) {
                  ref.read(loginFormProvider.notifier).setUserId(nextValue);
                })),
            ElevatedButton(
                child: const Text("Toggle Login"),
                onPressed: () {
                  if (user.userId == null) {
                    ref
                        .read(userProvider.notifier)
                        .login(loginForm.userId ?? "");
                  } else {
                    ref.read(userProvider.notifier).logout();
                  }
                }),
            Text(user.userId ?? "Not Logged In")
          ])
        ]));
  }
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'TEO Companion',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: const ProviderScope(child: LoginPage()),
    );
  }
}
