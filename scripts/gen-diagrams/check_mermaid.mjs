#!/usr/bin/env node
//  SPDX-License-Identifier: GPL-3.0-or-later
//
//  DOES EVERY FIGURE ON THESE PAGES ACTUALLY DRAW?
//
//  The pages carry mermaid SOURCE; the picture is made in the reader's browser.
//  So nothing here ever knew whether a figure could be drawn, and two that could
//  not were published for months: the page showed their source in a scrolling box,
//  which is exactly what the page is written to do when mermaid refuses.
//
//  This opens the generated pages in the browser mermaid runs in - the same
//  mermaid, at the version the page itself pins, over a local server so the
//  pages load as they will be served - and reports every figure that did not
//  come out as a picture. Invoke-Diagrams runs it before anything is published,
//  so a figure that cannot be drawn stops the publish instead of reaching the site.
//
//  Usage:  node check_mermaid.mjs <directory-or-file> [...]
//  Exit:   0 - every figure drew.  1 - at least one did not, or the check itself
//          could not run (no puppeteer, no network for the mermaid module).

import { createRequire } from 'node:module';
import { execSync } from 'node:child_process';
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const RENDER_TIMEOUT_MS = 60000;

//  Puppeteer is installed globally on the machine (see Install-Puppeteer in
//  tools/build-lib/prerequisites.ps1), not vendored beside this script, so it is
//  resolved from the global root rather than from here.
function loadPuppeteer() {
  try {
    return createRequire(import.meta.url)('puppeteer');
  } catch {
    const root = execSync('npm root -g', { encoding: 'utf8' }).trim();
    return createRequire(path.join(root, 'anchor.cjs'))('puppeteer');
  }
}

//  A page that sends the browser somewhere else (gen_diagrams: moved_page) has
//  no figures and navigates out from under the check. It is skipped by what it
//  is, rather than by name, so a new one needs no edit here.
const REDIRECT_RE = /<meta\s+http-equiv=["']refresh["']/i;

function isRedirect(file) {
  return REDIRECT_RE.test(fs.readFileSync(file, 'utf8'));
}

function htmlFilesUnder(target) {
  const stat = fs.statSync(target);
  if (stat.isFile()) return [target];
  return fs.readdirSync(target, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(target, entry.name);
    if (entry.isDirectory()) return htmlFilesUnder(full);
    return entry.isFile() && entry.name.endsWith('.html') ? [full] : [];
  });
}

const MIME = {
  '.html': 'text/html', '.css': 'text/css', '.js': 'text/javascript',
  '.svg': 'image/svg+xml', '.png': 'image/png', '.jpg': 'image/jpeg',
  '.gif': 'image/gif', '.mp4': 'video/mp4', '.ico': 'image/x-icon',
};

function serve(root) {
  const server = http.createServer((req, res) => {
    const rel = decodeURIComponent(req.url.split('?')[0]).replace(/^\/+/, '');
    const file = path.join(root, rel);
    if (!file.startsWith(root) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) {
      res.writeHead(404).end();
      return;
    }
    res.writeHead(200, { 'content-type': MIME[path.extname(file)] || 'application/octet-stream' });
    fs.createReadStream(file).pipe(res);
  });
  return new Promise((resolve) => {
    server.listen(0, '127.0.0.1', () => resolve({ server, port: server.address().port }));
  });
}

async function checkPage(browser, url, name) {
  const page = await browser.newPage();
  const problems = [];
  try {
    await page.goto(url, { waitUntil: 'load', timeout: RENDER_TIMEOUT_MS });
    const count = await page.$$eval('pre.mermaid', (els) => els.length);
    if (count === 0) return problems;
    //  The page renders its figures one at a time and marks each one when it is
    //  done, so waiting for the marks is waiting for exactly that work.
    await page.waitForFunction(
      () => [...document.querySelectorAll('pre.mermaid')].every((el) => el.dataset.rendered),
      { timeout: RENDER_TIMEOUT_MS },
    );
    const figures = await page.$$eval('pre.mermaid', (els) => els.map((el) => ({
      rendered: el.dataset.rendered,
      error: el.dataset.error || '',
      head: (el.dataset.source || el.textContent || '').trim().split('\n')[0],
    })));
    figures.forEach((fig, i) => {
      if (fig.rendered !== 'true') {
        problems.push(`${name}: figure ${i + 1} (${fig.head}) did not draw - ${fig.error || 'mermaid refused it'}`);
      }
    });
  } catch (e) {
    problems.push(`${name}: ${e.message}`);
  } finally {
    await page.close();
  }
  return problems;
}

async function main(targets) {
  if (targets.length === 0) {
    console.error('usage: node check_mermaid.mjs <directory-or-file> [...]');
    return 1;
  }
  const puppeteer = loadPuppeteer();
  const problems = [];
  let checked = 0;
  for (const target of targets) {
    const root = path.resolve(fs.statSync(target).isFile() ? path.dirname(target) : target);
    const files = htmlFilesUnder(path.resolve(target));
    const { server, port } = await serve(root);
    const browser = await puppeteer.launch({ args: ['--no-sandbox'] });
    try {
      for (const file of files) {
        if (isRedirect(file)) continue;
        const rel = path.relative(root, file).split(path.sep).join('/');
        problems.push(...await checkPage(browser, `http://127.0.0.1:${port}/${rel}`, rel));
        checked += 1;
      }
    } finally {
      await browser.close();
      server.close();
    }
  }
  if (problems.length) {
    console.error('The generated site has figures that do not draw:');
    problems.forEach((p) => console.error(`    ${p}`));
    console.error('Fix the figure in scripts/gen-diagrams/gen_diagrams.py - a page that '
                  + 'shows the source of a diagram is worse than a page with no diagram.');
    return 1;
  }
  console.log(`    every figure drew (${checked} page(s) checked)`);
  return 0;
}

process.exitCode = await main(process.argv.slice(2));
