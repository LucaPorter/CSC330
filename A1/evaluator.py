import os
import re
import shutil
import subprocess as sp
import zipfile
import tempfile

from pathlib import Path


def get_file(zip, prob_id, ext):
    if isinstance(zip, list):
        for z in zip:
            if (code := get_file(z, prob_id, ext)):
                return code
        return None

    def check_file(path):
        return path.name.lower() == f"prob{prob_id}.{ext}"

    path = None
    try:
        with zipfile.ZipFile(zip) as f:
            for file in sorted(f.namelist()):
                if not file.startswith("__") and not file.startswith("."):
                    p = Path(file)
                    if check_file(p):
                        print(f"- 📄 Found file {p.name} in {zip}")
                        with tempfile.NamedTemporaryFile(delete=False) as fo:
                            fo.write(f.read(file))
                            path = Path(fo.name)
    except (zipfile.BadZipFile, IsADirectoryError):
        p = Path(zip)
        if p.is_dir():
            if (f := (p / f"prob{prob_id}.{ext}")).exists():
                print(f"- 📄 Found file {f} as-is")
                path = f
        elif check_file(p):
            print(f"- 📄 Found file {p.name} as-is")
            path = p
    if not path:
        return None

    with open(path) as f:
        return f.read()


def ocaml_check_style(code, allowed_fns, ignore_patterns=[]):
    pats = [
        ("mut", r"(\b(ref|:=)\b)|(!\s*\b[\w']+\b)|:="),
        ("open", r"\bopen\b"),
        ("array", r"\[\||\|\]"),
        ("lib", r"\b([A-Z]\w*)\.(\w+)"),
        ("match", r"\bmatch\b"),
    ]
    pats = [(pi, re.compile(p)) for pi, p in pats]
    errors = []
    for li, l in enumerate(code.split("\n")):
        for pi, pat in pats:
            if pi in ignore_patterns: continue
            for i in pat.finditer(l):
                if pi == "lib" and i.group() in allowed_fns:
                    continue
                errors.append((li, pi, i.start(), i.group()))
    return errors


def ocaml_compile(prob_id, code, test_handler, tmp_dir):
    with open(tmp_dir / f"prob{prob_id}.ml", "w") as fo:
        fo.write(code)
    shutil.copy(test_handler, tmp_dir / "test.ml")
    try:
        out = sp.check_output(
            f"ocamlc prob{prob_id}.ml test.ml -o test.exe",
            stderr=sp.STDOUT,
            shell=True,
            cwd=tmp_dir
        ).decode('utf-8')
    except Exception as e:
        out = e.output.decode('utf-8')
    exe = tmp_dir / "test.exe"
    if out:
        errors = [i for i in out.split("\n") if i]
        return (exe if os.path.exists(exe) else None), errors
    return exe, []


def ocaml_run(code, tmp_dir, testcases,
              prob_id, test_handler, header="",
              points={}, style=None, ignore_style_patterns=[]):
    if style:
        style_errors = ocaml_check_style(code, style, ignore_style_patterns)
    else:
        style_errors = []
    exe, compile_errors = ocaml_compile(prob_id, header + code, test_handler, tmp_dir)

    results = {}
    if exe:
        for tc_id, (tc_input, tc_output) in enumerate(testcases):
            tc_name = tc_input.split()[0]
            total = sum(tc_input.startswith(tc_name + " ") for tc_input, _ in testcases)
            try:
                out = sp.check_output([exe, *(tc_input.split())],
                                      cwd=tmp_dir, stderr=sp.STDOUT)
                out = out.decode('utf-8').split('\n')
                can_capture = False
                l = []
                for i in out:
                    if i.startswith("[csc330_tester]"):
                        can_capture = True
                        i = i[16:]
                    if can_capture:
                        l.append(i.strip())
                out = '\n'.join(l).strip()
            except Exception as e:
                compile_errors.append(f"=> Unexpected evaluation error: {e}")
            tc_output = tc_output.strip()
            results.setdefault(tc_name, [0, "", 0])[0] += 1
            if out == tc_output:
                results[tc_name][1] += "✅"
                if (p := points.get(tc_name, 0)) > 0:
                    results[tc_name][2] += p / total
            else:
                results[tc_name][1] += "❌"
                print(
                    f"- ❌ Failed test {tc_name}.{tc_id}:\n",
                    f"   - Got:    {out}\n",
                    "" if out.startswith("!")
                    else f"   - Wanted: {tc_output}\n", sep="", end=""
                )
    return style_errors, compile_errors, results


def racket_run(code, tmp_dir, prob_id, test_handler, points={}):
    errors = []
    output = []
    proc = None
    try:
        with open(tmp_dir / f"prob{prob_id}.rkt", "w") as fo:
            code_fix = re.sub(
                r"^(#lang\s+racket\s*)$",
                r"\1\n(provide (all-defined-out))\n",
                code, flags=re.M
            )
            if code_fix == code:
                fo.write("#lang racket\n")
                fo.write("(provide (all-defined-out))\n")
                fo.write(code)
            else:
                fo.write(code_fix)
        shutil.copy(test_handler, tmp_dir / "test.rkt")
        proc = sp.run(["racket", tmp_dir / "test.rkt"],
                      stdout=sp.PIPE, stderr=sp.PIPE)
        for i in proc.stdout.decode('utf-8').split('\n'):
            if i.startswith("[csc330_tester]"):
                output.append(i[16:].strip().split())
            elif i.strip():
                errors.append(i.strip())
        for i in proc.stderr.decode('utf-8').split('\n'):
            if i.strip(): errors.append(i.strip())
    except Exception as e:
        errors.append(f"=> Unexpected evaluation error: {e}")
        if proc and proc.stderr:
            for i in proc.stderr.decode('utf-8').split('\n'):
                if i.strip(): errors.append(i.strip())
    results = {}
    for tc_name, solved, tot in output:
        pts = points.get(tc_name, 0) / float(tot) * int(solved)
        results[tc_name] = [tot, "✅" * int(solved) +
                                 "❌" * (int(tot) - int(solved)), pts]
    return [], errors, results


def ruby_run(code, tmp_dir, prob_id, test_handler, points={}):
    errors = []
    output = []
    proc = None
    try:
        with open(tmp_dir / f"prob{prob_id}.rb", "w") as fo:
            fo.write(code)
        shutil.copy(test_handler, tmp_dir / "test.rb")
        proc = sp.run(["ruby", "test.rb"],
                      stdout=sp.PIPE, stderr=sp.PIPE, cwd=tmp_dir)
        for i in proc.stdout.decode('utf-8').split('\n'):
            if i.startswith("[csc330_tester] !ERR"):
                errors.append(i[16:].strip())
            elif i.startswith("[csc330_tester]"):
                output.append(i[16:].strip().split())
            elif i.strip():
                errors.append(i.strip())
        for i in proc.stderr.decode('utf-8').split('\n'):
            if i.strip(): errors.append(i.strip())
    except Exception as e:
        errors.append(f"=> Unexpected evaluation error: {e}")
        if proc and proc.stderr:
            for i in proc.stderr.decode('utf-8').split('\n'):
                if i.strip(): errors.append(i.strip())
    results = {}
    for tc_name, solved, tot in output:
        pts = points.get(tc_name, 0) / float(tot) * int(solved)
        results[tc_name] = [tot, "✅" * int(solved) +
                                 "❌" * (int(tot) - int(solved)), pts]
    return [], errors, results


def apl_run(code, tmp_dir, test_handler, points={}):
    errors = []
    output = []
    proc = None
    try:
        with open(tmp_dir / f"test.apl", "w") as fo:
            fo.write("⎕io ← 0\n")
            fo.write(code)
            fo.write("\n")
            with open(test_handler) as f:
                fo.write(f.read())
        os.system(f"cat {tmp_dir}/test.apl")
        os.system(f"dyalogscript {tmp_dir}/test.apl")
        proc = sp.run(["dyalogscript", "test.apl"],
                      stdout=sp.PIPE, stderr=sp.PIPE, cwd=tmp_dir)
        for i in proc.stdout.decode('utf-8').splitlines():
            if i.strip().startswith("[csc330_tester]"):
                output.append(i.strip()[16:].strip().split())
            elif i.strip():
                errors.append(i.strip())
        for i in proc.stderr.decode('utf-8').splitlines():
            if i.strip(): errors.append(i)
    except Exception as e:
        errors.append(f"=> Unexpected evaluation error: {e}")
        if proc and proc.stderr:
            for i in proc.stderr.decode('utf-8').splitlines():
                if i.strip(): errors.append(i)
    results = {}
    for tc_name, *score in output:
        tot = len(score)
        solved = sum(1 for i in score if i == "1")
        pts = points.get(tc_name, 0) / float(tot) * int(solved)
        results[tc_name] = [tot, "✅" * int(solved) +
                                 "❌" * (int(tot) - int(solved)), pts]
    return [], errors, results


def summarize(style_errors, compile_errors, results):
    if style_errors:
        print("\n### Style warnings:\n")
        for ln, pn, cl, m in style_errors:
            print(f"- ⚠️ Line {ln}:{cl}: found disallowed pattern {pn}: {m}")
    if compile_errors:
        print("\n### Errors and warnings:\n")
        for e in compile_errors:
            print(f"- ⛔️ {e}")
    if results:
        print("\n### Summary:\n")
        total_all = 0
        for name, (total, log, pts) in results.items():
            ok = sum(1 for i in log if i == "✅")
            print(f"- 🧪 **{name}**: {log} ({ok}/{total}); estimated points: {pts:.1f}")
            total_all += pts
        print(f"\n* 🏆 Total points: {total_all:.1f}")
