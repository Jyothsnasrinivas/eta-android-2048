# Eta Android 2048

![eta-android-2048 preview](eta-android-2048.gif)

## Prerequisites
- JDK 8 or above

## Running

1. Clone the sources.

   ```
   $ git clone https://github.com/Jyothsnasrinivas/eta-android-2048.git
   $ cd eta-android-2048
   ```

2. Add your Android SDK path in `local.properties`.

   local.properties
   ```
   sdk.dir=[SDK-Path-Here]
   ```

3. Build the APK.

   ```
   $ ./gradlew assembleDebug
   ```

## Testing on mobile or emulator

If you want to test it on your phone first enable the developer mode and run the command below. Similarly for testing on emulator.

```
$ ./gradlew installDebug
```

## Release APK

If you want to build the release apk which has proguard enabled, run the command below:

```
$ ./gradlew assembleRelease
```
