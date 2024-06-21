# 定义项目的名称为 `egame`
PROJECT = egame

# 项目的描述，这里是一个Web MUD游戏
PROJECT_DESCRIPTION = A web MUD game demo

# 项目的版本号，这里设置为0.1.0
PROJECT_VERSION = 0.1.0

# 编译Erlang代码时的选项：
# -Werror 将所有警告当作错误处理
# +debug_info 包含调试信息
# +warn_export_vars 警告未被使用的导出变量
# +warn_shadow_vars 警告变量名遮蔽
# +warn_obsolete_guard 警告使用过时的守卫
ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

# 项目依赖的列表，这些是项目运行所依赖的其他Erlang库或应用程序：
DEPS = cowboy jiffy bcrypt ailib aiconf aihtml aidb

# 指定依赖项的具体提交版本或标签：
# Cowboy 依赖的提交版本,网络通信
dep_cowboy_commit = 2.12.0

# Bcrypt 依赖的提交版本,加密
dep_bcrypt_commit = 1.2.2

# Jiffy 依赖的提交版本,json处理
dep_jiffy_commit = 1.1.2

# 自定义库
# ailib 依赖的Git仓库和标签
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.4.1

# aiconf 依赖的Git仓库和标签
dep_aiconf = git https://github.com/DavidAlphaFox/aiconf.git tag-0.1.3

# aidb 依赖的Git仓库和版本
dep_aidb = git https://github.com/DavidAlphaFox/aidb.git v0.3.4

# aihtml 依赖的Git仓库和版本
dep_aihtml = git https://github.com/DavidAlphaFox/aihtml.git v0.3.4

# 包含erlang.mk以使用构建系统的功能
include erlang.mk


#如果此处你的erlang版本逐渐升高，记得对官方库进行版本设置进行升级