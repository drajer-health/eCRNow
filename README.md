# eCR Now Developer Setup Guide for macOS

This guide provides instructions for setting up your development environment for the eCR Now application on macOS.

(N.B., I asked an AI to write most of it, hit me up if any issues...)

## Prerequisites

All instructions in this guide are specific to macOS. The eCR Now application requires:

- Java 17
- Maven 3.3.x or higher
- PostgreSQL 10.x or higher

## Setting Up Your Development Environment

### 1. Installing SDKMAN

[SDKMAN](https://sdkman.io/) is a tool for managing parallel versions of multiple Software Development Kits on most Unix-based systems. We'll use it to install Java and Maven.

#### Standard Installation (for Bash/Zsh users)

Open Terminal and run:

```bash
curl -s "https://get.sdkman.io" | bash
```

Follow the instructions on screen to complete the installation.

To verify the installation, open a new terminal window and run:

```bash
sdk version
```

<details>
<summary>For Fish users</summary>
If you use the Fish shell, you'll need to install the Fish SDKMAN plugin using Fisher:

1. First, install Fisher if you don't have it already:

```fish
curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
```

2. Then install the SDKMAN plugin:

```fish
fisher install reitzig/sdkman-for-fish
```
3. Set the environment variable SDKMAN_DIR:

```fish
set -Ux SDKMAN_DIR ~/.sdkman
```
4. Verify the installation:

```fish
sdk version
```
</details>

### 2. Installing Java 17

With SDKMAN installed, you can now install Temurin Java 17. We're using Temurin because it's a FOSS JVM and Oracle
won't try to make us pay for it later.

```bash
sdk install java 17.0.9-tem
```

This will install the latest Temurin Java 17 version available through SDKMAN.

To verify the installation:

```bash
java -version
```

You should see output indicating you're using Temurin Java 17.

### 3. Installing Maven

Install Maven using SDKMAN (do not use Homebrew as it may install a different Java version):

```bash
sdk install maven 3.9.10
```

To verify the installation:

```bash
mvn --version
```

This should show the Maven version and confirm it's using the Java 17 installation we set up earlier.


## Building the Project

### 1. Clone the Repository

```bash
git clone https://github.com/drajer-health/eCRNow.git
cd eCRNow
```

### 2. Create a PostgreSQL Database

Create a PostgreSQL database for the project:

```bash
createdb -h localhost -p 5432 -U your_username ecrnow_db
```

Or use a tool like pgAdmin to create the database.

### 3. Configure the Application

Update the database configuration in `src/main/resources/application.properties` as needed.

### 4. Build the Application

Build the application using Maven:

```bash
mvn clean install
```

### 5. Run the Application

Run the application using:

```bash
java -Djdbc.username=your_username -Djdbc.password=your_password -Dsecurity.key=your_security_key -jar ./target/ecr-now.war
```

Replace `your_username`, `your_password`, and `your_security_key` with your actual PostgreSQL credentials and a security key for encrypting sensitive information.

## Additional Resources

For more detailed information about the application, refer to:

- [The original README](Original_README.md)
- [eCRNow App Configuration Guide](https://github.com/drajer-health/eCRNow/blob/master/documents/eCR%20Now%20App%20Configuration%20Guide_Release3.0.docx)
- [Security Configuration](https://github.com/drajer-health/eCRNow/blob/master/SecurityConfiguration.md)
- [Performance Tuning](https://github.com/drajer-health/eCRNow/blob/master/PerformanceTuning.md)