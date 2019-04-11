SET PATH=D:\DevTools\Tools\LAZAPK~1\android-tools;C:\Program Files (x86)\Java\jdk1.7.0_03\bin
del LazAndroidOrmDemo.apk
rd bin /s /q
rd raw /s /q
rd gen /s /q
mkdir bin
mkdir bin\classes
mkdir gen
mkdir raw
mkdir raw\lib
mkdir raw\lib\armeabi
copy libs\armeabi\*.so raw\lib\armeabi\
D:\DevTools\Tools\LAZAPK~1\android-tools\aapt.exe p  -f -M D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\AndroidManifest.xml -F D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\LazAndroidOrmDemo.ap_ -I D:\DevTools\Tools\LAZAPK~1\android-tools\android.jar -S D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\res -m -J D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\gen -A D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\assets D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\raw
copy D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\gen\cn\questsoft\lazandroidormdemo\R.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\R.java
C:\PROGRA~1\Java\JDK17~1.0_7\bin\javac.exe -source 1.6 -target 1.6 -verbose -encoding UTF8 -classpath D:\DevTools\Tools\LAZAPK~1\android-tools\android.jar;D:\DevTools\Tools\LAZAPK~1\android-tools\android-support-v4.jar;D:\DevTools\Tools\LAZAPK~1\android-tools\annotations.jar -d D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\classes D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\R.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\App.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\Controls.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jButton.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jCheckBox.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jCommons.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jDialogYN.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jEditText.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jImageFileManager.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jImageView.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jListView.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jPanel.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jPreferences.java D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\src\cn\questsoft\lazandroidormdemo\jTextView.java
C:\PROGRA~1\Java\JDK17~1.0_7\bin\java.exe -Djava.ext.dirs=D:\DevTools\Tools\LAZAPK~1\android-tools\ -jar D:\DevTools\Tools\LAZAPK~1\android-tools\dx.jar --dex --output=D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\classes.dex D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\classes
C:\PROGRA~1\Java\JDK17~1.0_7\bin\java.exe -classpath D:\DevTools\Tools\LAZAPK~1\android-tools\sdklib.jar com.android.sdklib.build.ApkBuilderMain D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\LazAndroidOrmDemo-unsigned.apk -u -z D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\LazAndroidOrmDemo.ap_ -f D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\classes.dex 
C:\PROGRA~1\Java\JDK17~1.0_7\bin\jarsigner.exe -sigalg MD5withRSA -digestalg SHA1 -keystore D:\DevTools\Tools\LAZAPK~1\debugkey.keystore -keypass 123456 -storepass 123456 -signedjar D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\LazAndroidOrmDemo-unaligned.apk D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\LazAndroidOrmDemo-unsigned.apk debugkey
D:\DevTools\Tools\LAZAPK~1\android-tools\zipalign.exe -v 4 D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\bin\LazAndroidOrmDemo-unaligned.apk D:\github\LAZAND~1\project\LAZAND~1\LAZAND~1\LazAndroidOrmDemo.apk
rd bin /s /q
rd raw /s /q
rd gen /s /q
