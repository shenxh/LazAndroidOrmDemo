set JAVA_HOME=C:\Program Files\Java\jdk1.7.0_79
set PATH=%JAVA_HOME%\bin;%PATH%
set JAVA_TOOL_OPTIONS=-Duser.language=en
cd D:\github\LazAndroidOrmDemo\project\LazAndroidProject\LazAndroidOrmDemo
keytool -genkey -v -keystore LazAndroidOrmDemo-release.keystore -alias lazandroidormdemoaliaskey -keyalg RSA -keysize 2048 -validity 10000 < D:\github\LazAndroidOrmDemo\project\LazAndroidProject\LazAndroidOrmDemo\keytool_input.txt
