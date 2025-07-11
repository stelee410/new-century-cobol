# New Century COBOL

A modern COBOL program generator tool.

## Features

- Quick COBOL program skeleton generation
- Interactive user input
- Automatic program ID and author information generation
- Complete COBOL program structure included

## Installation and Usage

### Method 1: Direct Python Script Execution

```bash
python cbl.py new
```

### Method 2: Using Launch Script

```bash
python cbl new
```

## Usage Examples

After running the command, the tool will prompt you to enter the following information:

1. **Program ID (Program-ID)**: Your COBOL program name
2. **Author ID (Author-ID)**: Program author information

### Example Session

```bash
$ python cbl new
=== COBOL Program Generator ===

Please enter Program ID (Program-ID): HELLO-WORLD
Please enter Author ID (Author-ID): John Doe
✓ COBOL program successfully created: HELLO-WORLD.cbl
✓ Program ID: HELLO-WORLD
✓ Author ID: John Doe
```

## Generated File Structure

The generated COBOL program contains the following standard structure:

- **IDENTIFICATION DIVISION**: Program identification information
- **ENVIRONMENT DIVISION**: Environment configuration
- **DATA DIVISION**: Data definitions
- **PROCEDURE DIVISION**: Program logic

## File Permission Settings

If you want to run `./cbl new` directly, please set execution permissions:

```bash
chmod +x cbl
```

## System Requirements

- Python 3.6+
- Terminal with UTF-8 encoding support

## License

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