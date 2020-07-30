# FSLM
Fishbone Salesforce Commerce Cloud Log Merger in Delphi Tokyo.

When developing with SFCC, logs are output from multiple site ID instances with multiple categories and log levels.
In the file, log information is distributed and recorded in multiple files, regardless of the logs arranged in chronological order.
Even if one log file is opened, not all log information output from the program to be debugged is available.
In the case of a log that is output as a separate category as a dedicated log for linking with an external system, a special category-file is output.
Since the log is output, it may not be recorded in the normal log file.
Therefore, normally you will open multiple log files and check the operation of the program, but the logs from multiple programs
It's very difficult work because they are mixed.
Therefore, multiple log files required for debugging are merged and arranged in chronological order, and the site ID of the site ID where the program to be debugged is running
By extracting only the log, it helps to easily trace the log of the program to be debugged.

　SFCCにて開発する場合、複数のカテゴリ―やログレベルにて複数のサイトIDのインスタンスからログが出力する。
ファイル内では時系列に並んでいるログにも関わらず複数のファイルにログ情報が分散して記録されるため、
一つのログファイルを開いてもデバッグ対象のプログラムから出力されたログ情報が全てあるとは限らない。
外部システムとの連携専用ログとして別カテゴリ―として出力しているログの場合は、専用のカテゴリ―ファイルに
ログが出力されるため、通常のログファイルに記録がないことがある。
　そのため、通常は複数のログファイルを開いてプログラムの動作を確認することとなるが、複数のプログラムからのログが
入り混じっているためとても大変な作業となる。
そこで、デバッグに必要とする複数のログファイルをマージして時系列順に並べ、デバッグ対象のプログラムが動作しているサイトIDの
ログだけを抽出することにより、デバッグ対象のプログラムのログの追跡を容易できるよう支援する。

