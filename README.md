# LazAndroidOrmDemo
A Lazarus Android mORMot example.
The backend database uses MariaDB ，Android client is developed using laz4android1.6 + mORMot .
The intermediate services layer is developed using laz4android1.6 + mORMot + ZeosDBO.
This example is based on an android application that has been running for 3 years. The image data uploaded in the background has exceeded 80G and is still running normally.
Now I'm going to open source it.

这是一个由laz4android开发的安卓应用示例。
后台数据库使用了MariaDB,安卓客户端使用laz4android1.6 + mORMot开发,通过中间服务层进行JSON格式的数据交互.中间服务层由laz4android1.6 + mORMot + ZeosDBO开发.
这个示例是基于本人开发的一个运行了3年之久的安卓应用而来.那个安卓应用的后台上传图片数据已经超过了80G,目前还在运行使用中.云服务器挂在了阿里云上面.
我现在把它开源出来.为lazarus社区作一些贡献。

1: laz4android1.6.0 + FPC3.0.0
https://sourceforge.net/projects/laz4android

2: MariaDB-10.1.18
https://downloads.mariadb.org

3: mORMot 1.18
https://synopse.info

4: ZeosDB 7.2.1-rc
https://sourceforge.net/projects/zeoslib

5: LazAndroidModuleWizard(LAMW)
https://github.com/jmpessoa/lazandroidmodulewizard

![image](https://github.com/shenxh/LazAndroidOrmDemo/blob/master/screenshots/20190411_221426.png)

