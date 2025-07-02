#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
COBOL程序生成器命令行工具
使用方法: python cbl.py new
"""

import sys
import os
import argparse
from datetime import datetime


def get_user_input(prompt, default=""):
    """获取用户输入"""
    if default:
        user_input = input(f"{prompt} (默认: {default}): ").strip()
        return user_input if user_input else default
    else:
        return input(f"{prompt}: ").strip()


def validate_cobol_identifier(identifier, max_length=30):
    """验证COBOL标识符是否符合规范"""
    if not identifier:
        return False, "标识符不能为空"
    
    if len(identifier) > max_length:
        return False, f"标识符长度不能超过{max_length}个字符"
    
    # COBOL标识符必须以字母开头，只能包含字母、数字和连字符
    if not identifier[0].isalpha():
        return False, "标识符必须以字母开头"
    
    for char in identifier:
        if not (char.isalnum() or char == '-'):
            return False, "标识符只能包含字母、数字和连字符(-)"
    
    return True, ""


def validate_filename(filename, max_length=255):
    """验证文件名是否符合规范"""
    if not filename:
        return False, "文件名不能为空"
    
    if len(filename) > max_length:
        return False, f"文件名长度不能超过{max_length}个字符"
    
    # 检查文件名是否包含非法字符
    invalid_chars = '<>:"/\\|?*'
    for char in invalid_chars:
        if char in filename:
            return False, f"文件名不能包含字符: {char}"
    
    return True, ""


def generate_cobol_skeleton(program_id, author_id):
    """生成COBOL程序壳"""
    current_date = datetime.now().strftime("%Y-%m-%d")
    current_time = datetime.now().strftime("%H:%M:%S")
    
    cobol_template = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {program_id}.
       AUTHOR. {author_id}.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
       
       PROGRAM-DONE.
           STOP RUN.
"""
    return cobol_template


def create_new_cobol_program(output_dir=None):
    """创建新的COBOL程序"""
    print("=== COBOL程序生成器 ===")
    print()
    
    # 获取用户输入
    program_id = get_user_input("请输入程序ID (Program-ID)")
    if not program_id:
        print("错误: 程序ID不能为空")
        return False
    
    author_id = get_user_input("请输入作者ID (Author-ID)")
    if not author_id:
        print("错误: 作者ID不能为空")
        return False
    
    # 转换为大写
    program_id = program_id.upper()
    author_id = author_id.upper()
    
    # 验证COBOL标识符规范
    is_valid, error_msg = validate_cobol_identifier(program_id)
    if not is_valid:
        print(f"错误: 程序ID不符合COBOL规范 - {error_msg}")
        return False
    
    is_valid, error_msg = validate_cobol_identifier(author_id)
    if not is_valid:
        print(f"错误: 作者ID不符合COBOL规范 - {error_msg}")
        return False
    
    # 生成文件名
    filename = f"{program_id}.cbl"
    
    # 验证文件名规范
    is_valid, error_msg = validate_filename(filename)
    if not is_valid:
        print(f"错误: 文件名不符合规范 - {error_msg}")
        return False
    
    # 处理输出目录
    if output_dir:
        # 确保输出目录存在
        try:
            os.makedirs(output_dir, exist_ok=True)
        except Exception as e:
            print(f"错误: 无法创建输出目录 {output_dir}: {e}")
            return False
        
        # 构建完整文件路径
        filepath = os.path.join(output_dir, filename)
    else:
        filepath = filename
    
    # 检查文件是否已存在
    if os.path.exists(filepath):
        overwrite = input(f"文件 {filepath} 已存在，是否覆盖? (y/N): ").strip().lower()
        if overwrite != 'y':
            print("操作已取消")
            return False
    
    # 生成COBOL程序内容
    cobol_content = generate_cobol_skeleton(program_id, author_id)
    
    # 写入文件
    try:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(cobol_content)
        print(f"✓ COBOL程序已成功创建: {filepath}")
        print(f"✓ 程序ID: {program_id}")
        print(f"✓ 作者ID: {author_id}")
        return True
    except Exception as e:
        print(f"错误: 无法创建文件 {filepath}: {e}")
        return False


def main():
    """主函数"""
    parser = argparse.ArgumentParser(
        description="COBOL程序生成器",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
示例用法:
  python cbl.py new                   创建新的COBOL程序
  python cbl.py new --output src/     在指定目录创建COBOL程序
        """
    )
    
    parser.add_argument(
        'command',
        choices=['new'],
        help='要执行的命令'
    )
    
    parser.add_argument(
        '--output', '-o',
        type=str,
        help='指定输出目录 (可选)'
    )
    
    args = parser.parse_args()
    
    if args.command == 'new':
        success = create_new_cobol_program(args.output)
        sys.exit(0 if success else 1)


if __name__ == "__main__":
    main() 