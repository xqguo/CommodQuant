# CheatSheet

Some references for tools. 

## [The Missing Semester of Your CS Education](https://missing.csail.mit.edu/)

This is a good collections of lectures on tools that will get you started. 

## [Scoop](https://github.com/lukesampson/scoop)

This is quite cool to get unix cmds into windows 10 without a lof of unix knowledge. Using powershell felt as if a unix shell. Highly recommended for windows user.  Some recommended packages:

```
~ $ scoop list
Installed apps:

  7zip 19.00
  anaconda3 2019.10 [extras]
  aria2 1.35.0-1
  dark 3.11.2
  gopass 1.8.6
  gpg 2.2.19
  grep 2.5.4
  lessmsi 1.6.91
  pshazz 0.2019.08.07
  vim 8.2
  which 2.20
```

if you install anaconda with scoop, run activate to activate the vistual python environment.  

## SSH

- To use ssh on 443 port, you can add the following in ~/.ssh/config

  ```sshconfig
  Host github.com
    HostName ssh.github.com
    Port 443
    User git
    PreferredAuthentications publickey
  Host gitlab.com
    Hostname altssh.gitlab.com
    User git
    Port 443
    PreferredAuthentications publickey
  ```

- [Copy ssh id](https://www.ssh.com/ssh/copy-id)

```bash
ssh -i ~/.ssh/mykey user@host
```

## Git
- [Gitlab git cheat sheet](https://about.gitlab.com/images/press/git-cheat-sheet.pdf)
- Use submoduels to have nested git repositories. [ref](https://www.vogella.com/tutorials/GitSubmodules/article.html)

```console
git clone --recursive [URL to Git repo]
git submodule update --init
git submodule update --init --recursive
git pull --recurse-submodules
git submodule update --remote
```

- Create a clean git history [ref](https://tecadmin.net/delete-commit-history-in-github/)

```console
git checkout --orphan temp_branch
git add -A
git commit -am "the first commit"
git branch -D master
git branch -m master
git push -f origin master
```

- Or Simply rebase the whole thing

```console
git rebase -i --root
git push --force
```

- gitignore is used to avoid some file to be uploaded to repo

[Learn Git](https://www.atlassian.com/git/tutorials/saving-changes/gitignore)

- diff local master with remote

```console
git diff master origin/master
``` 

## [Dotnet CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/?tabs=netcore2x) and [Paket](https://fsprojects.github.io/Paket/)

- sometime vs run into build issues, and the cli provides a more consistent experience. To restore, build, test, publish etc. Paket is integrated smoothly, but you might need to run paket install before building the 1st time.

- use dotnet tool install paket globally

```console
dotnet tool install paket --global
```
- Use paket to isntall/restore/update packages. [Get started](https://fsprojects.github.io/Paket/getting-started.html). To start from scratch if packages cannot be downloaded or installed properly, you can [clear cache](https://fsprojects.github.io/Paket/paket-clear-cache.html)

```console
paket install
paket update
paket clear-cache
dotnet restore
dotnet build -c Release
dotnet vstest Test.dll #run test on a prebuilt test dll
dotnet test --filter DisplayName~getPrice #run XunitTest with a filter
dotnet publish -c Release -o outputdir
```
- Use jupyter notebook for F#

For people familiar with jupyter notebook/lab, you can follow this [guide](https://devblogs.microsoft.com/dotnet/net-interactive-is-here-net-notebooks-preview-2) and enable F# for it.  

```
dotnet tool install --global Microsoft.dotnet-interactive
dotnet interactive jupyter install #from Anaconda console
jupyter kernelspec list
```
- for a even more controlled build process, use [Fake](https://fake.build/)


## PowerShell

- create csv and select cloumns

```pwsh
(type .\navdb.csv | Select-String "," ) | ConvertFrom-Csv | select "Path","Frequency"
```

- add user path entry and remove duplicates
  adapted from [this post](https://itluke.online/2018/07/16/how-to-remove-duplicates-from-your-path-environment-variable-with-powershell/).  

```pwsh
$CurrentPath = [Environment]::GetEnvironmentVariable('Path','User')
$TargetPath = $CurrentPath+";path1"
$SplittedPath = $TargetPath -split ';'
$CleanedPath = $SplittedPath | Sort-Object -Unique
$NewPath = $CleanedPath -join ';'
[Environment]::SetEnvironmentVariable('Path', $NewPath,'User')
```

- delete directory by force

```
rm -r -fo somedir
```


- cmd to copy folder
```console
robocopy src des /MIR
```

or

```console
xcopy /S src des
```

or

```
cp -Recurse -Force src dst
```

## WSL

- add ca cert from windows to wsl's mono

```bash
certmgr -ssl https://nuget.org
certmgr -ssl https://github.com
```

- mount network drive

```
sudo mkdir /mnt/f
sudo mount -t drvfs F: /mnt/f
```

## Unix commands

- [find](http://man7.org/linux/man-pages/man1/find.1.html) files with certain pattern and run cmd on them, e.g. change dos to unix txt format for all .sh .vim and .py files.

```bash
find -iregex '.*\.\(sh\|vim\|py\)$' -exec dos2unix {} \;
```

- [grep](https://www.gnu.org/software/grep/manual/grep.html) filename with contents in current directory and run some cmds on them via [xargs](http://man7.org/linux/man-pages/man1/xargs.1.html).

```bash
grep -rl PATTERN  . | xargs ls -l
```

## [GPG](https://www.gnupg.org/)

[A useful cheat sheet](https://guides.library.illinois.edu/data_encryption/gpgcheatsheet)

Selected common comands:

```console
gpg --list-secret-keys 
#export private and public key
gpg --export-secret-keys -a $ID > my-private-key.asc
gpg --armor --output public-key.gpg --export user@example.com
gpg --import private_or_pub_key.asc
gpg --delete-key 
gpg --delete-secret-key
#extend expiry requires secret key
gpg --edit-key $id
gpg> expire
gpg> save
#trust imported secret key
gpg> trust 
#encrypt file
gpg -ers $id file
```


## [GoPass](https://www.gopass.pw/)

- Gopass is an password manager. Use it instead of pass for better functionality and windows integration.

```console
gopass some_entry
gopass clone git_repo
```

## [wget](https://www.gnu.org/software/wget/manual/wget.html)

- download all jpg files from a reference page

```console
wget -r -A .jpy url
```
