# New Century COBOL

一个现代化的COBOL程序生成器工具。

## 功能特性

- 快速生成COBOL程序壳
- 交互式用户输入
- 自动生成程序ID和作者信息
- 包含完整的COBOL程序结构

## 安装和使用

### 方法1: 直接运行Python脚本

```bash
python cbl.py new
```

### 方法2: 使用启动脚本

```bash
python cbl new
```

## 使用示例

运行命令后，工具会提示您输入以下信息：

1. **程序ID (Program-ID)**: 您的COBOL程序名称
2. **作者ID (Author-ID)**: 程序作者信息

### 示例会话

```bash
$ python cbl new
=== COBOL程序生成器 ===

请输入程序ID (Program-ID): HELLO-WORLD
请输入作者ID (Author-ID): 张三
✓ COBOL程序已成功创建: HELLO-WORLD.cbl
✓ 程序ID: HELLO-WORLD
✓ 作者ID: 张三
```

## 生成的文件结构

生成的COBOL程序包含以下标准结构：

- **IDENTIFICATION DIVISION**: 程序标识信息
- **ENVIRONMENT DIVISION**: 环境配置
- **DATA DIVISION**: 数据定义
- **PROCEDURE DIVISION**: 程序逻辑

## 文件权限设置

如果您想直接运行 `./cbl new`，请设置执行权限：

```bash
chmod +x cbl
```

## 系统要求

- Python 3.6+
- 支持UTF-8编码的终端

## 许可证

MIT License

**New Century COBOL**

Nobody loves COBOL, yet it has never truly been replaced. For decades, COBOL has quietly powered the world's most critical business systems—banking, insurance, government, aviation. Every transaction, every identity verification may carry its trace. *New Century COBOL* is a tribute to this underestimated language and an exploration into the future: bringing legacy code into conversation with the modern world.

This project is nothing but but serves more as a tribute to the wisdom of the past.

It will includes:
- script to manage the cobol project, skeleton, compile, link and run. Or to submit the JCL to zOS
- the COBOL code and explaination for who want to learn this program language.

Minimun requirement:

- GNUCobol

Recommended requirement:

- z explorer for VS studio
- zowe CLI