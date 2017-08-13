#! python3

import os
import sys
import re

def get_version(text):
    """ Основываясь на данных в .cabal файле,
        возвращает текущую версию приложения. """
    version_pattern = re.compile(r"version: *([0-9.]*)")
    version = version_pattern.search(text)
    if version is not None:
        return version.groups()[0]
    else:
        return ""

def create_app_dir(current):
    """ Создает директорию с исполняемым файлом проекта. """
    with open(current + "DDChat.cabal", "r", encoding="utf-8") as f:
        cabal_text = f.read()
        version = get_version(cabal_text)
    dir_name = "Sheer-Linux-" + version
    dir_path = current + dir_name
    if not os.path.exists(dir_path):
        os.makedirs(dir_path)
    return dir_path

def replace_special_notation(main_file, current):
    """ Заменяет специальные обозначения в Main файле на
        соответствующее содержимое указанных файлов. """
    def worker(out_data, notation, path):
        with open(path, "r", encoding="utf8") as f:
            to_write = f.read().replace("\n", "\\n")
        return out_data.replace(notation, to_write)

    css_path = current + os.sep.join(["static", "css"]) + os.sep
    fonts_css = css_path + "fonts.css"
    login_css = css_path + "login.css"
    with open(main_file, "r", encoding="utf8") as f:
        previous = f.read()
    data = worker(previous, "__FONTS__", fonts_css)
    data = worker(data, "__LOGIN__", login_css)
    with open(main_file, "w", encoding="utf8") as f:
        f.write(data)
    return previous

def to_initial_state(main_file, previous):
    """ После необходимых команд, возвращает
        Main файл в свое изначальное состояние. """
    with open(main_file, "w", encoding="utf8") as f:
        f.write(previous)

def build_app(current, app_dir_path):
    """ Компилирует программу и копирует выполняемый файл в директорию,
        созданную функцией CreateAppDir."""
    os.system("stack build")
    src = current + os.sep.join([".stack-work", "dist", "x86_64-linux",
                                 "Cabal-1.24.2.0", "build", "DDChat-exe",
                                 "DDChat-exe"])
    dst = app_dir_path + os.sep + "Sheer"
    os.system("cp " + src + " " + dst)
    os.system("stack clean")

def main(main_file, current):
    # Создаем директорию с будущим выполняемым файлом.
    app_dir_path = create_app_dir(current)
    # Производим замену
    previous = replace_special_notation(main_file, current)
    # Собираем проект
    build_app(current, app_dir_path)
    # Возвращаем Main файл в изначальное состояние
    to_initial_state(main_file, previous)

if __name__ == "__main__":
    current = os.getcwd() + os.sep
    main_file = current + os.sep.join(["app", "Main.hs"])
    main(main_file, current)
